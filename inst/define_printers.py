import sympy
from sympy import *
from sympy.printing.pretty.pretty import PrettyPrinter
from sympy.printing.pretty.pretty_symbology import pretty_use_unicode
from sympy.printing.pretty.stringpict import prettyForm

class CaracasPrettyPrinter(PrettyPrinter):
    def _print_ExpBase(self, e):
        return self._helper_print_function(e.func, e.args)

    def _print_Exp1(self, e):      
        return prettyForm("exp(1)")

def print_caracas_unicode(expr, **settings):   
    settings['use_unicode'] = True
    
    pp = CaracasPrettyPrinter(settings)
    #    breakpoint()
    # XXX: this is an ugly hack, but at least it works
    use_unicode = pp._settings['use_unicode']
    uflag = pretty_use_unicode(use_unicode)

    try:
        return print(pp.doprint(expr))
    finally:
        pretty_use_unicode(uflag)

def print_caracas(expr, **settings):   
    settings['use_unicode'] = False
    
    pp = CaracasPrettyPrinter(settings)
    #    breakpoint()
    # XXX: this is an ugly hack, but at least it works
    use_unicode = pp._settings['use_unicode']
    uflag = pretty_use_unicode(use_unicode)

    try:
        return print(pp.doprint(expr))
    finally:
        pretty_use_unicode(uflag)
        
