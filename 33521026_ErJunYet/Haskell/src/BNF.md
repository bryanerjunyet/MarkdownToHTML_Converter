# BNF for Markdown Document

This document defines the Backus-Naur Form (BNF) for a subset of the Markdown specification that outlines the hierarchical structure and relationships between different Markdown elements such as `BlockElement` and `FreeText` (`PlainText` and `TextModifier`).

```BNF

<Document>          ::= <Elements> | <Elements> <Document>

<Elements>          ::= <BlockElement> | <ParagraphElement>


<ParagraphElement>  ::= <Paragraph>
<BlockElement>      ::= <Header> | <BlockQuote> | <BlockCode> | <FootNoteReference> | <OrderedList> | <Table> | <Image>


<Paragraph>         ::= <FreeText> <NewLine> | <FreeText> <Paragraph>

<FreeText>          ::= <PlainText> | <TextModifier>
<PlainText>         ::= <String> | <Space> <PlainText>
<TextModifier>      ::= <Bold> | <Italic> | <Strikethrough> | <Link> | <InlineCode> | <Footnote>

<Italic>            ::= '_' <PlainText> '_'
<Bold>              ::= '**' <PlainText> '**'
<Strikethrough>     ::= '~~' <PlainText> '~~'
<Link>              ::= '[' <PlainText> ']' '(' <PlainText> ')'
<InlineCode>        ::= '`' <PlainText> '`'
<FootNote>          ::= '[^' <Integer> ']'


<Header>            ::= <PlainHeader> | <AlternativeHeader>
<PlainHeader>       ::= <Hashes> <Space> <FreeText> <NewLine>
<AlternativeHeader> ::= <AlternativeHeader1> | <AlternativeHeader2>
<AlternativeHeader1>::= <Equals> <Space> <FreeText> <NewLine>
<AlternativeHeader2>::= <Dashes> <Space> <FreeText> <NewLine>

<BlockQuote>        ::= '>' <Paragraph> | <Paragraph> <BlockQuote>
<BlockCode>         ::= '```' <PlainText> <Newline> '```'
<FootNoteReference> ::= <FootNote> ':' <PlainText>

<OrderedList>       ::= OrderedListItem | <OrderedListItem> <NewLine> <OrderedList>
<OrderedListItem>   ::= <Integer> '.' <Space> <FreeText> | <Sublist>
<Sublist>           ::= <Indent> <OrderedListItem> | <Indent> <OrderedListItem> <NewLine> <Sublist>

<Table>             ::= <TableHeader> <Newline> <TableSeparator> <Newline> <TableRows>
<TableHeader>       ::= <TableRow>
<TableRows>         ::= <TableRow> | <TableRow> <Newline> <TableRows>
<TableRow>          ::= <Space> '|' <TableCells> '|'
<TableCells>        ::= <TableCell> | <TableCell> '|' <TableCells>
<TableCell>         ::= <Space> <FreeText> <Space>
<TableSeparator>    ::= <Space> '|' <Dashes> '|' <Dashes> '|' <Dashes> '|' <Space>
<TableDashes>       ::= '---' | '-' <TableDashes>

<Image>             ::= '![' <PlainText> ']' '(' <PlainText> <Space> <PlainText> ')'


<Space>             ::= ' ' | ' ' <Space>
<Indent>            ::= '    '
<NewLine>           ::= '\n'
<Hashes>            ::= '#' | '##' | '###' | '####' | '#####' | '######'
<Equals>            ::= '==' | '=' <Equals>
<Dashes>            ::= '--' | '-' <Dashes>
<String>            ::= <Character> | <Character> <String>
<Character>         ::= 'a' | 'b' | 'c' | ... | '0' | '1' | '2' | ... 


```