import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// This file contains code that was generated with the assistance of ChatGPT and Copilot.
// Some parts of the code were created or modified using these tools to improve efficiency and accuracy.
// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const titleInput = document.getElementById("title-input") as HTMLInputElement;
const saveBtn = document.getElementById("save-btn")! as HTMLButtonElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

const titleInputStream$: Observable<Action> = fromEvent(titleInput, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((newTitle) => (s) => { 
        const updatedTitle = newTitle.trim() || "Untitled";
        const updatedHTML = s.HTML.replace(/<title>(.*?)<\/title>/, `<title>${updatedTitle}</title>`);
        return ({ ...s, title: updatedTitle, HTML: updatedHTML});
     }),
);

const saveBtnStream$: Observable<Action> = fromEvent(saveBtn, "click").pipe(
    map(() => {
        console.log('Save button clicked');
        return (s) => ({ ...s, save: true });
    })
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            const updatedHTML = data.html.replace(/<title>(.*?)<\/title>/, `<title>${s.title}</title>`);
            return {
                ...s,
                HTML: updatedHTML,
            };
        }),
        first(),
    );
}

function saveHTML(s: State): Observable<State> {
    console.log("Saving HTML", s);  
    const updatedHTML = s.HTML.replace(/<title>(.*?)<\/title>/, `<title>${s.title}</title>`);
    return ajax({
        url: "/api/saveHTML",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: updatedHTML,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            console.log("HTML Saved", data);  
            return {
                ...s,
                save: true,
            };
        }),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: "Untitled",
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, titleInputStream$, saveBtnStream$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(resetState)(reducer);
                return newReducer;
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return newState.save? saveHTML(newState) : getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                    htmlOutput.style.whiteSpace = "normal";
                } else {
                    htmlOutput.textContent = value.HTML;
                    htmlOutput.style.whiteSpace = "pre-wrap";
                }
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
