import pytest
from interpreter import Interpreter, BinOp, UnOp, Number, NodeVisitor
from interpreter import Token, TokenType, Parser


@pytest.fixture(scope="function")
def interpreter():
    return Interpreter()


@pytest.fixture(scope="function")
def parser():
    return Parser()


class TestInterpreter:
    interpreter = Interpreter()

    def test_add(self, interpreter):
        assert interpreter.eval("2+2") == 4

    def test_sub(self, interpreter):
        assert interpreter.eval("2-2") == 0

    def test_mul(self, interpreter):
        assert interpreter.eval("2*2") == 4
        assert interpreter.eval("2+2*2") == 6

    def test_div(self, interpreter):
        assert interpreter.eval("2/2") == 1
        assert interpreter.eval("2+2/2") == 3

    def test_brackets(self, interpreter):
        assert interpreter.eval("2*(2+2)") == 8
        assert interpreter.eval("2*((2+2)+(3*2))") == 20

    def test_un_plus(self, interpreter):
        assert interpreter.eval("++++2") == 2
        assert interpreter.eval("+2+++2") == 4

    def test_un_minus(self, interpreter):
        assert interpreter.eval("-2") == -2
        assert interpreter.eval("+2+--2") == 4

    def test_NodeVisitor(self):
        assert NodeVisitor().visit() is None

    def test_add_with_letter(self, interpreter):
        with pytest.raises(SyntaxError):
            interpreter.eval("2+a")
        with pytest.raises(SyntaxError):
            interpreter.eval("t+2")

    def test_wrong_operator(self, interpreter):
        with pytest.raises(SyntaxError):
            interpreter.eval("2&3")

    def test_invalid_token_order(self, parser):
        parser._current_token = Token(TokenType.NUMBER, "2")
        with pytest.raises(SyntaxError):
            parser.check_token(TokenType.OPERATOR)

    def test_invalid_factor(self, interpreter):
        with pytest.raises(SyntaxError):
            interpreter.eval("2++")

    def test_BinOp_invalid_operator(self, interpreter):
        with pytest.raises(ValueError):
            interpreter.visit_binop(BinOp(Number(Token(TokenType.NUMBER, "2")), Token(TokenType.OPERATOR, "&"),
                                          Number(Token(TokenType.NUMBER, "2"))))

    def test_UnOp_invalid_operator(self, interpreter):
        with pytest.raises(ValueError):
            interpreter.visit_unop(UnOp(Token(TokenType.OPERATOR, "&"), Number(Token(TokenType.NUMBER, "2"))))

    def test_expr_break(self, interpreter):
        assert interpreter.eval("2+2*2/2") == 6

    def test_ast_Number_str(self):
        assert Number(Token(TokenType.NUMBER, "2")).__str__() == f"Number (Token(TokenType.NUMBER, 2))"

    def test_ast_BinOp_str(self):
        assert BinOp(Number(Token(TokenType.NUMBER, "2")), Token(TokenType.OPERATOR, "+"),
                     Number(Token(TokenType.NUMBER, "2"))).__str__() == \
               f"BinOp+ (Number (Token(TokenType.NUMBER, 2)), Number (Token(TokenType.NUMBER, 2)))"

    def test_ast_UnOp_str(self):
        assert UnOp(Token(TokenType.OPERATOR, "+"), Number(Token(TokenType.NUMBER, "2"))).__str__() == \
               f"UnOp+ (Number (Token(TokenType.NUMBER, 2)))"

    @pytest.mark.parametrize(
        "interpreter, code", [(interpreter, "2 + 2"),
                              (interpreter, "2 +2 "),
                              (interpreter, " 2+2")]
    )
    def test_add_spaces(self, interpreter, code):
        assert interpreter.eval(code) == 4
