from sympy import Symbol
from sympy.printing.pretty.pretty import PrettyPrinter, prettyForm, pretty_atom

class CaracasPrettyPrinter(PrettyPrinter):
    _default_settings = {
        'use_unicode': False,
    }

    def __init__(self, settings={}):
        super(PrettyPrinter, self).__init__(settings)

    def _print_ExpBase(self, e):
        # TODO should exp_polar be printed differently?
        #      what about exp_polar(0), exp_polar(1)?
        base = prettyForm(pretty_atom('Exp1', 'e'))
        return base ** self._print(e.args[0])

    def _print_Exp1(self, e):
        return prettyForm(pretty_atom('Exp1', 'e'))


def print_caracas(expr):
    print(CaracasPrettyPrinter().doprint(expr))

