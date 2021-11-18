import antlr4 from "antlr4";
import LSFJSLogicsLexer from './LSFJSLogicsLexer';
import LSFJSLogicsParser from './LSFJSLogicsParser';

class AnnotatingErrorListener extends antlr4.error.ErrorListener {
    constructor(annotations) {
        super();
        this.annotations = annotations;
    }

    syntaxError(recognizer, offendingSymbol, line, column, msg, e) {
        this.annotations.push({
            row: line - 1,
            column: column,
            text: msg,
            type: "error"
        });
    }
}

window.antlrLSFValidate = function antlrLSFValidate(input) {
    var stream = new antlr4.InputStream(input);
    var lexer = new LSFJSLogicsLexer(stream);
    var tokens = new antlr4.CommonTokenStream(lexer);
    var parser = new LSFJSLogicsParser(tokens);
    var annotations = [];
    var listener = new AnnotatingErrorListener(annotations)
    parser.removeErrorListeners();
    parser.addErrorListener(listener);
    parser.script();
    return annotations;
}
