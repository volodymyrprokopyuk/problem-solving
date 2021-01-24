(: xmllint --html --encode utf8 --xmlout data.html > data.xml :)

string-join(("Category", "Title", "Author", "Year", "Price"), ";"),
for $b in //book
    return string-join(($b/@category, $b/title, $b/author, $b/year, $b/price), ";")
