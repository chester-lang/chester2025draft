__all__ = ['example']

# Don't look below, you will not understand this Python code :) I don't.

from js2py.pyjs import *
# setting scope
var = Scope( JS_BUILTINS )
set_global_object(var)

# Code follows:
var.registers(['$nonPyName', 'sayHello', 'someVariable', 'Rectangle'])
@Js
def PyJsHoisted_sayHello_(name, this, arguments, var=var):
    var = Scope({'name':name, 'this':this, 'arguments':arguments}, var)
    var.registers(['name'])
    var.get('console').callprop('log', ((Js('Hello, ')+var.get('name'))+Js('!')))
PyJsHoisted_sayHello_.func_name = 'sayHello'
var.put('sayHello', PyJsHoisted_sayHello_)
@Js
def PyJsHoistedNonPyName(this, arguments, var=var):
    var = Scope({'this':this, 'arguments':arguments}, var)
    var.registers([])
    var.get('console').callprop('log', Js('Non-Py names like $ can be used too!'))
PyJsHoistedNonPyName.func_name = '$nonPyName'
var.put('$nonPyName', PyJsHoistedNonPyName)
@Js
def PyJsHoisted_Rectangle_(w, h, this, arguments, var=var):
    var = Scope({'w':w, 'h':h, 'this':this, 'arguments':arguments}, var)
    var.registers(['h', 'w'])
    var.get(u"this").put('w', var.get('w'))
    var.get(u"this").put('h', var.get('h'))
PyJsHoisted_Rectangle_.func_name = 'Rectangle'
var.put('Rectangle', PyJsHoisted_Rectangle_)
var.put('someVariable', Js({'a':Js(1.0),'b':Js(2.0)}))
pass
pass
pass
@Js
def PyJs_anonymous_0_(this, arguments, var=var):
    var = Scope({'this':this, 'arguments':arguments}, var)
    var.registers([])
    return (var.get(u"this").get('w')*var.get(u"this").get('h'))
PyJs_anonymous_0_._set_name('anonymous')
var.get('Rectangle').put('prototype', Js({'getArea':PyJs_anonymous_0_}))
var.put('x', var.get('Rectangle').create(Js(10.0), Js(10.0)))


# Add lib to the module scope
example = var.to_python()