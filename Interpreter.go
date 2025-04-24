package main

import (
	"fmt"
	"go/constant"
	"math/big"
	"strings"
)

type Frame map[string]Expr
type Bindings []Frame

type Expr interface {
	Value(Bindings) Expr
	String() string
}

// *****************************************************

type TextExpr string

func (t TextExpr) Value(b Bindings) Expr {
	return t
}

func (t TextExpr) String() string {
	return constant.MakeString(string(t)).ExactString()
}

func Text(text string) Expr {
	return TextExpr(text)
}

// *****************************************************

type IntExpr struct {
	number *big.Int
}

func (i IntExpr) Value(b Bindings) Expr {
	return i
}

func (i IntExpr) String() string {
	return fmt.Sprint(i.number)
}

func Int(num string) IntExpr {
	number := new(big.Int)
	number.SetString(num, 10)
	return IntExpr{number}
}

// *****************************************************

type AddExpr struct {
	left, right Expr
}

func (a AddExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := a.left.Value(b).(IntExpr)
	rightnum, rightIsnum := a.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	result := big.NewInt(0)
	result.Add(leftnum.number, rightnum.number)

	return IntExpr{result}
}

func (a AddExpr) String() string {
	return fmt.Sprintf("(%v + %v)", a.left, a.right)
}

func Add(x, y Expr) AddExpr {
	return AddExpr{x, y}
}

// *****************************************************

type SubExpr struct {
	left, right Expr
}

func (s SubExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := s.left.Value(b).(IntExpr)
	rightnum, rightIsnum := s.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	result := big.NewInt(0)
	result.Sub(leftnum.number, rightnum.number)

	return IntExpr{result}
}

func (s SubExpr) String() string {
	return fmt.Sprintf("(%v - %v)", s.left, s.right)
}

func Sub(x, y Expr) SubExpr {
	return SubExpr{x, y}
}

// *****************************************************

type MulExpr struct {
	left, right Expr
}

func (m MulExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := m.left.Value(b).(IntExpr)
	rightnum, rightIsnum := m.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	result := big.NewInt(0)
	result.Mul(leftnum.number, rightnum.number)

	return IntExpr{result}
}

func (m MulExpr) String() string {
	return fmt.Sprintf("(%v * %v)", m.left, m.right)
}

func Mul(x, y Expr) MulExpr {
	return MulExpr{x, y}
}

// *****************************************************

type DivExpr struct {
	left, right Expr
}

func (d DivExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := d.left.Value(b).(IntExpr)
	rightnum, rightIsnum := d.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	result := big.NewInt(0)
	if rightnum.number.Cmp(big.NewInt(0)) == 0 {
		return IntExpr{result}
	}
	result.Div(leftnum.number, rightnum.number)

	return IntExpr{result}
}

func (d DivExpr) String() string {
	return fmt.Sprintf("(%v / %v)", d.left, d.right)
}

func Div(x, y Expr) DivExpr {
	return DivExpr{x, y}
}

// *****************************************************

type ModExpr struct {
	left, right Expr
}

func (m ModExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := m.left.Value(b).(IntExpr)
	rightnum, rightIsnum := m.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	result := big.NewInt(0)
	if rightnum.number.Cmp(big.NewInt(0)) == 0 {
		return IntExpr{leftnum.number}
	}
	result.Mod(leftnum.number, rightnum.number)

	return IntExpr{result}
}

func (m ModExpr) String() string {
	return fmt.Sprintf("(%v %% %v)", m.left, m.right)
}

func Mod(x, y Expr) ModExpr {
	return ModExpr{x, y}
}

// *****************************************************

type BoolExpr bool

func Bool(b bool) BoolExpr {
	return BoolExpr(b)
}

func (x BoolExpr) Value(b Bindings) Expr {
	return x
}

func (x BoolExpr) String() string {
	if x {
		return "TRUE"
	} else {
		return "FALSE"
	}
}

// *****************************************************

type AndExpr struct {
	left, right Expr
}

func (a AndExpr) Value(b Bindings) Expr {
	abool, aIsbool := a.left.Value(b).(BoolExpr)
	bbool, bIsbool := a.right.Value(b).(BoolExpr)
	if !aIsbool {
		panic("TYPE ERROR")
	}
	if !abool {
		return Bool(false)
	}
	if !bIsbool {
		panic("TYPE ERROR")
	}
	if bbool {
		return Bool(true)
	}

	return Bool(false)
}

func (s AndExpr) String() string {
	return fmt.Sprintf("(%v AND %v)", s.left, s.right)
}

func And(x, y Expr) AndExpr {
	return AndExpr{x, y}
}

// *****************************************************

type OrExpr struct {
	left, right Expr
}

func (o OrExpr) Value(b Bindings) Expr {
	abool, aIsbool := o.left.Value(b).(BoolExpr)
	bbool, bIsbool := o.right.Value(b).(BoolExpr)
	if !aIsbool {
		panic("TYPE ERROR")
	}
	if abool {
		return Bool(true)
	}
	if !bIsbool {
		panic("TYPE ERROR")
	}
	if bbool {
		return Bool(true)
	}
	return Bool(false)
}

func (s OrExpr) String() string {
	return fmt.Sprintf("(%v OR %v)", s.left, s.right)
}

func Or(x, y Expr) OrExpr {
	return OrExpr{x, y}
}

// *****************************************************

type NotExpr struct {
	expr Expr
}

func (n NotExpr) Value(b Bindings) Expr {
	abool, aIsbool := n.expr.Value(b).(BoolExpr)
	if !aIsbool {
		panic("TYPE ERROR")
	}
	if abool {
		return Bool(false)
	}
	return Bool(true)
}

func (s NotExpr) String() string {
	return fmt.Sprintf("NOT %v", s.expr)
}

func Not(x Expr) NotExpr {
	return NotExpr{x}
}

// *****************************************************

type LtExpr struct {
	left, right Expr
}

func (l LtExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := l.left.Value(b).(IntExpr)
	rightnum, rightIsnum := l.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	if leftnum.number.Cmp(rightnum.number) == -1 {
		return Bool(true)
	}

	return Bool(false)
}

func (l LtExpr) String() string {
	return fmt.Sprintf("(%v < %v)", l.left, l.right)
}

func Lt(x, y Expr) LtExpr {
	return LtExpr{x, y}
}

// *****************************************************

type LeExpr struct {
	left, right Expr
}

func (l LeExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := l.left.Value(b).(IntExpr)
	rightnum, rightIsnum := l.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	if leftnum.number.Cmp(rightnum.number) == -1 || leftnum.number.Cmp(rightnum.number) == 0 {
		return Bool(true)
	}

	return Bool(false)
}

func (l LeExpr) String() string {
	return fmt.Sprintf("(%v <= %v)", l.left, l.right)
}

func Le(x, y Expr) LeExpr {
	return LeExpr{x, y}
}

// ********************************************************

type EqExpr struct {
	left, right Expr
}

func (e EqExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := e.left.Value(b).(IntExpr)
	rightnum, rightIsnum := e.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	if leftnum.number.Cmp(rightnum.number) == 0 {
		return Bool(true)
	}

	return Bool(false)
}

func (e EqExpr) String() string {
	return fmt.Sprintf("(%v == %v)", e.left, e.right)
}

func Eq(x, y Expr) EqExpr {
	return EqExpr{x, y}
}

// *****************************************************

type GeExpr struct {
	left, right Expr
}

func (g GeExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := g.left.Value(b).(IntExpr)
	rightnum, rightIsnum := g.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	if leftnum.number.Cmp(rightnum.number) == 1 || leftnum.number.Cmp(rightnum.number) == 0 {
		return Bool(true)
	}

	return Bool(false)
}

func (g GeExpr) String() string {
	return fmt.Sprintf("(%v >= %v)", g.left, g.right)
}

func Ge(x, y Expr) GeExpr {
	return GeExpr{x, y}
}

// *****************************************************

type GtExpr struct {
	left, right Expr
}

func (g GtExpr) Value(b Bindings) Expr {
	leftnum, leftIsnum := g.left.Value(b).(IntExpr)
	rightnum, rightIsnum := g.right.Value(b).(IntExpr)

	if !leftIsnum || !rightIsnum {
		panic("TYPE ERROR")
	}

	if leftnum.number.Cmp(rightnum.number) == 1 {
		return Bool(true)
	}

	return Bool(false)
}

func (g GtExpr) String() string {
	return fmt.Sprintf("(%v > %v)", g.left, g.right)
}

func Gt(x, y Expr) GtExpr {
	return GtExpr{x, y}
}

// *****************************************************

type Stmt interface {
	Execute(b Bindings)
	String() string
}

type Block []Stmt

func (block Block) Execute(b Bindings) {
	b = b.NewFrame()
	for _, stmt := range block {
		stmt.Execute(b)
	}
}

func (block Block) String() string {
	var b strings.Builder

	for _, stmt := range block {
		for _, line := range strings.Split(stmt.String(), "\n") {
			fmt.Fprintln(&b, "    "+line)
		}
	}

	return "{\n" + b.String() + "}"
}

func (b Bindings) NewFrame() Bindings {
	return append(b, make(Frame))
}

func (b Bindings) Add(name string, expr Expr) {
	lastFrame := b[len(b)-1]
	_, haveOld := lastFrame[name]

	if haveOld {
		panic(fmt.Sprint("VARIABLE ", name, " ALREADY PRESENT"))
	}

	lastFrame[name] = expr
}

func (b Bindings) Change(name string, expr Expr) {
	for i := len(b) - 1; i >= 1; i-- {
		frame := b[i]
		_, haveOld := frame[name]

		if !haveOld {
			continue
		}

		frame[name] = expr
		return
	}

	panic(fmt.Sprint("VARIABLE ", name, " NOT FOUND"))
}

func (b Bindings) Get(name string) Expr {
	for i := len(b) - 1; i >= 0; i-- {
		frame := b[i]
		value, haveOld := frame[name]

		if !haveOld {
			continue
		}

		return value
	}

	panic(fmt.Sprint("VARIABLE ", name, " NOT FOUND"))
}

// ********************************************************

type VarExpr string

func (v VarExpr) Value(b Bindings) Expr {
	return b.Get(string(v))
}

func (v VarExpr) String() string {
	return string(v)
}

func (v VarExpr) Change(b Bindings, ex Expr) {
	b.Change(v.String(), ex)
}

func Var(s string) VarExpr {
	return VarExpr(s)
}

// ********************************************************

type AddVarStmt struct {
	name VarExpr
	expr Expr
}

func AddVar(name string, expr Expr) AddVarStmt {
	return AddVarStmt{Var(name), expr}
}

func (a AddVarStmt) Execute(b Bindings) {
	value := a.expr.Value(b)
	b.Add(a.name.String(), value)
}

func (a AddVarStmt) String() string {
	return fmt.Sprintf("%v := %v", a.name, a.expr)
}

// *********************************************************

type Cell interface {
	Expr
	Change(Bindings, Expr)
}

type ChangeStmt struct {
	cell Cell
	expr Expr
}

func (c ChangeStmt) Execute(b Bindings) {
	c.cell.Change(b, c.expr.Value(b))
}

func (c ChangeStmt) String() string {
	return fmt.Sprintf("%v = %v", c.cell, c.expr)
}

func Change(cell Cell, expr Expr) ChangeStmt {
	return ChangeStmt{cell, expr}
}

// ********************************************************

type Pointer struct {
	bindings Bindings
	cell     Cell
}

func (p Pointer) Value(b Bindings) Expr {
	return p // Указатель -- это вид литерала
}

func (p Pointer) String() string {
	return fmt.Sprintf("<POINTER TO %v>", p.cell)
}

// *******************************************************

type RefExpr struct {
	cell Cell
}

func Ref(cell Cell) RefExpr {
	return RefExpr{cell}
}

func (r RefExpr) Value(b Bindings) Expr {
	bindings := make(Bindings, len(b))
	copy(bindings, b)

	return Pointer{bindings: bindings, cell: r.cell}
}

func (r RefExpr) String() string {
	return fmt.Sprintf("&%v", r.cell)
}

// *******************************************************

type DerefExpr struct {
	pointer Expr
}

func (d DerefExpr) Change(b Bindings, ex Expr) {
	point, Ispoint := d.pointer.Value(b).(Pointer)

	if !Ispoint {
		panic("TYPE ERROR")
	}

	point.bindings.Change(point.cell.String(), ex)
}

func (d DerefExpr) Value(b Bindings) Expr {
	point, Ispoint := d.pointer.Value(b).(Pointer)

	if !Ispoint {
		panic("TYPE ERROR")
	}

	return point.cell.Value(point.bindings)
}

func (d DerefExpr) String() string {
	return fmt.Sprintf("*%v", d.pointer)
}

func Deref(point Expr) DerefExpr {
	return DerefExpr{point}
}

// *******************************************************

type Function struct {
	name string
	args []string
	body Block
}

type Builtin struct {
	name string
	body func(...Expr) Expr
}

func (f Function) Value(b Bindings) Expr {
	return f
}

func (f Function) String() string {
	var b strings.Builder

	fmt.Fprint(&b, "FUNCTION[")

	for i, arg := range f.args {
		if i == 0 {
			fmt.Fprint(&b, arg)
		} else {
			fmt.Fprint(&b, ", ", arg)
		}
	}

	fmt.Fprint(&b, "] ")
	fmt.Fprint(&b, f.body)

	return b.String()
}

func (f Builtin) Value(b Bindings) Expr {
	return f
}

func (f Builtin) String() string {
	return fmt.Sprintf("<BUILTIN %v>", f.name)
}

// *******************************************************

type CallExpr struct {
	expr Expr
	args []Expr
}

func Call(expr Expr, args ...Expr) CallExpr {
	return CallExpr{expr, args}
}

func (c CallExpr) Value(b Bindings) Expr {
	value := c.expr.Value(b)

	var values []Expr

	for _, item := range c.args {
		item = item.Value(b)
		values = append(values, item)
	}

	function, Isfunction := value.(Function)
	builtin, Isbuiltin := value.(Builtin)

	if Isfunction {
		bind := Bindings{b[0]}.NewFrame()

		for index := range values {
			bind.Add(function.args[index], values[index])
		}

		bind.Add(function.name, nil)
		function.body.Execute(bind)

		return bind.Get(function.name)

	} else if Isbuiltin {

		return builtin.body(values...)

	}

	panic("TYPE ERROR")

}

func (c CallExpr) String() string {
	var b strings.Builder

	fmt.Fprint(&b, "CALL")
	fmt.Fprint(&b, "[")
	fmt.Fprint(&b, c.expr)

	for _, arg := range c.args {
		fmt.Fprint(&b, ", ", arg)
	}

	fmt.Fprint(&b, "]")

	return b.String()
}

// *******************************************************

type EvalStmt struct {
	expr Expr
}

func Eval(expr Expr) EvalStmt {
	return EvalStmt{expr}
}

func (e EvalStmt) Execute(b Bindings) {
	e.expr.Value(b)
}

func (e EvalStmt) String() string {
	return e.expr.String()
}

// *******************************************************

type Definition AddVarStmt

func Const(name string, expr Expr) Definition {
	return Definition(AddVar(name, expr))
}

func Define(name string, args []string, body ...Stmt) Definition {
	return Definition(AddVar(name, Function{
		name: name,
		args: args,
		body: Block(body),
	}))
}

func (d Definition) String() string {
	return fmt.Sprintf("DEFINE %v", AddVarStmt(d))
}

// *******************************************************

type Program []Definition

func (p Program) Run(builtins []Builtin) {
	bind := Bindings{}.NewFrame()

	for _, value := range builtins {
		bind.Add(value.name, value.Value(bind))
	}
	for _, value := range p {
		AddVarStmt(value).Execute(bind)
	}
	Eval(Call(Var("main"))).Execute(bind)
}

func (p Program) String() string {
	var answer strings.Builder

	for index, x := range p {
		fmt.Fprint(&answer, x.String(), "\n")

		if index != len(p)-1 {
			fmt.Fprint(&answer, "\n")
		}
	}

	return answer.String()
}

// *******************************************************

type IfStmt struct {
	condition Expr
	ifTrue    Block
}

func (IF IfStmt) Execute(b Bindings) {
	value, ok := IF.condition.Value(b).(BoolExpr)

	if !ok {
		panic("TYPE ERROR")
	}
	if value {
		IF.ifTrue.Execute(b)
	}
}

func (IF IfStmt) String() string {
	return fmt.Sprintf("IF %v %v", IF.condition, IF.ifTrue)
}

func If(condition Expr, ifTrue Block) IfStmt {
	return IfStmt{condition, ifTrue}
}

// *******************************************************

type IfElseStmt struct {
	condition Expr
	ifTrue    Block
	ifFalse   Stmt
}

func (IFELSE IfElseStmt) Execute(b Bindings) {
	value, ok := IFELSE.condition.Value(b).(BoolExpr)

	if !ok {
		panic("TYPE ERROR")
	}

	if value {
		IFELSE.ifTrue.Execute(b)
	} else {
		IFELSE.ifFalse.Execute(b)
	}
}

func (IFELSE IfElseStmt) String() string {
	return fmt.Sprintf("IF %v %v ELSE %v", IFELSE.condition, IFELSE.ifTrue, IFELSE.ifFalse)
}

func IfElse(condition Expr, ifTrue Block, ifFalse Stmt) IfElseStmt {
	return IfElseStmt{condition, ifTrue, ifFalse}
}

// *******************************************************

type WhileStmt struct {
	condition Expr
	body      Block
}

func (WHILE WhileStmt) Execute(b Bindings) {
	value, ok := WHILE.condition.Value(b).(BoolExpr)

	if !ok {
		panic("TYPE ERROR")
	}

	for value {
		WHILE.body.Execute(b)
		value, ok = WHILE.condition.Value(b).(BoolExpr)
		if !ok {
			panic("TYPE ERROR")
		}
	}
}

func (WHILE WhileStmt) String() string {
	return fmt.Sprintf("WHILE %v %v", WHILE.condition, WHILE.body)
}

func While(condition Expr, body Block) WhileStmt {
	return WhileStmt{condition, body}
}

// *******************************************************

type LiteralArray struct {
	slice *[]Expr
}

func (l LiteralArray) Value(b Bindings) Expr {
	return l
}

func (l LiteralArray) String() string {
	var answer strings.Builder

	fmt.Fprint(&answer, "ARRAY[")

	for i, arg := range *l.slice {
		if i == 0 {
			fmt.Fprint(&answer, arg)
		} else {
			fmt.Fprint(&answer, ", ", arg)
		}
	}
	fmt.Fprint(&answer, "]")
	return answer.String()
}

// *******************************************************

type ArrayExpr struct {
	elements []Expr
}

func Array(x ...Expr) ArrayExpr {
	return ArrayExpr{x}
}

func (a ArrayExpr) Value(b Bindings) Expr {
	var array []Expr
	for _, arg := range a.elements {
		array = append(array, arg.Value(b))
	}
	return LiteralArray{&array}
}

func (a ArrayExpr) String() string {
	var answer strings.Builder

	fmt.Fprint(&answer, "ARRAY[")

	for i, arg := range a.elements {
		if i == 0 {
			fmt.Fprint(&answer, arg)
		} else {
			fmt.Fprint(&answer, ", ", arg)
		}
	}
	fmt.Fprint(&answer, "]")
	return answer.String()
}

// *******************************************************

type IndexExpr struct {
	arrayExpr Expr
	index     Expr
}

func Index(array, index Expr) IndexExpr {
	return IndexExpr{array, index}
}

func (i IndexExpr) Value(b Bindings) Expr {
	array, Isarray := i.arrayExpr.Value(b).(LiteralArray)
	index, isInt := i.index.Value(b).(IntExpr)

	if !Isarray || !isInt {
		panic("TYPE ERROR")
	}

	if len(*array.slice) <= int(index.number.Int64()) || int(index.number.Int64()) < 0 {
		panic("ARRAY ERROR")
	}

	return (*array.slice)[index.number.Int64()]
}

func (i IndexExpr) Change(b Bindings, value Expr) {
	array, Isarray := i.arrayExpr.Value(b).(LiteralArray)
	index, isInt := i.index.Value(b).(IntExpr)

	if !Isarray || !isInt {
		panic("TYPE ERROR")
	}

	if len(*array.slice) <= int(index.number.Int64()) || int(index.number.Int64()) < 0 {
		panic("ARRAY ERROR")
	}
	(*array.slice)[index.number.Int64()] = value
}

func (i IndexExpr) String() string {
	return fmt.Sprintf("INDEX[%v, %v]", i.arrayExpr, i.index)
}

// *******************************************************

type LengthExpr struct {
	arrayExpr Expr
}

func Length(array Expr) LengthExpr {
	return LengthExpr{array}
}

func (l LengthExpr) Value(b Bindings) Expr {
	array, ok := l.arrayExpr.Value(b).(LiteralArray)

	if !ok {
		panic("TYPE ERROR")
	}

	var counter int
	counter = len(*array.slice)
	answer := big.NewInt(int64(counter))

	return IntExpr{answer}
}

func (l LengthExpr) String() string {
	return fmt.Sprintf("LENGTH[%v]", l.arrayExpr)
}

// *******************************************************

type PopExpr struct {
	arrayExpr Expr
}

func Pop(array Expr) PopExpr {
	return PopExpr{array}
}

func (p PopExpr) Value(b Bindings) Expr {
	array, ok := p.arrayExpr.Value(b).(LiteralArray)

	if !ok {
		panic("TYPE ERROR")
	}

	length := len(*array.slice)
	if length == 0 {
		panic("ARRAY ERROR")
	}
	list := *array.slice
	answer := list[length-1]
	*array.slice = list[:length-1]
	return answer
}

func (p PopExpr) String() string {
	return fmt.Sprintf("POP[%v]", p.arrayExpr)
}

// *******************************************************

type AppendStmt struct {
	arrayExpr Expr
	elements  []Expr
}

func Append(array Expr, elements ...Expr) AppendStmt {
	return AppendStmt{array, elements}
}

func (a AppendStmt) Execute(b Bindings) {
	array, ok := a.arrayExpr.Value(b).(LiteralArray)

	if !ok {
		panic("TYPE ERROR")
	}

	slice := *array.slice
	for _, arg := range a.elements {
		slice = append(slice, arg.Value(b))
	}
	*array.slice = slice
}

func (a AppendStmt) String() string {
	var answer strings.Builder
	fmt.Fprint(&answer, a.arrayExpr, " <- ")

	for i, arg := range a.elements {
		if i == 0 {
			fmt.Fprint(&answer, arg)
		} else {
			fmt.Fprint(&answer, ", ", arg)
		}
	}
	return answer.String()
}

// *******************************************************

func main() {
	//bindings := Bindings{}.NewFrame().NewFrame()

	/*
		addFunc := Function{
			name: "add",
			args: []string{"x", "y"},
			body: Block{
				Change(Var("add"), Add(Var("x"), Var("y"))),
			},
		}
		bindings.Add("add", addFunc)
		result := Call(Var("add"), Int("2"), Int("2"))
	*/

	/*
		x := Var("x")
		bindings.Add(x.String(), Bool(true))

		condition := x
		ifTrue := Block{
			Eval(Add(Int("2"), Int("31"))),
		}
		ifFalse := Block{
			Eval(Add(Int("1"), Int("3"))),
		}
		ifSt := IFELSE(condition, ifTrue, ifFalse)
		ifSt.Execute(bindings)
		fmt.Println(ifSt)
	*/

	/*
		prog := Program{
			Const("foo", Add(Int("2"), Int("3"))),

			Define("square", []string{"x"},
					Change(Var("square"), Mul(Var("x"), Var("x"))),
			),

			Define("main", nil,
					Eval(Call(Var("print"), Call(Var("square"), Var("foo")))),
			),
		}

		fmt.Println(result.Value(bindings))
	*/

	/*

		AddVar("foo", Int("1")).Execute(bindings)
		Change(Var("foo"), Add(Var("foo"), Int("5"))).Execute(bindings)
		fmt.Println(Var("foo").Value(bindings))
	*/

	/*
		array := Array(Add(Int("2"), Int("5")), Int("76"), Mul(Int("15"), Int("20")))

		bindings.Add("foo", nil)

		Block {
			Change(Var("foo"), array),
			Eval(Pop(Var("foo"))),
			Append(Var("foo"), Int("8"), Int("15")),
			Eval(Pop(Var("foo"))),
		}.Execute(bindings)
		fmt.Println(Length(Var("foo")).Value(bindings))
		fmt.Println(Index(Var("foo"), Int("1")).Value(bindings))
		Index(Var("foo"),Int("1")).Change(bindings, Int("0"))
		fmt.Println(Var("foo").Value(bindings))
	*/
}
