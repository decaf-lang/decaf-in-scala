package decaf.tac

import decaf.annot.MemberVarSymbol

/**
  * Virtual table. In TAC, a named virtual table consists the following items (in order):
  * - the pointer to the virtual table of its parent class (if any), or null (if none)
  * - the class name (a string literal)
  * - labels of all member methods (static methods are EXCLUDED), which include those inherited from
  * super classes. For these inherited/overriden items, the offsets in this virtual table MUST be consistent with
  * those ones (virtual table of the parent class), for example:
  * {{{
  *   VTABLE(_Animal) {
  *     <empty>
  *     Animal
  *     _Animal.GetMom;       <-- offset 8 (byte)
  *     _Animal.GetHeight;    <-- offset 12
  *     _Animal.InitAnimal;   <-- offset 16
  *   }
  *
  *   VTABLE(_Cow) {
  *     _Animal
  *     Cow
  *     _Animal.GetMom;       <-- inherited from _Animal, offset 8
  *     _Cow.GetHeight;       <-- override _Animal's GetHeight, offset 12
  *     _Animal.InitAnimal;   <-- inherited from _Animal, offset 16
  *     _Cow.InitCow;         <-- newly defined
  *     _Cow.IsSpottedCow;    <-- newly defined
  *   }
  * }}}
  * Note that each item takes 4 bytes, and the offsets 8, 12, and 16 are consistent.
  *
  * For our convenience, we also store symbols of all member variables, but these are NOT necessary.
  *
  * @param name          virtual table name
  * @param className     class name
  * @param parent        parent class virtual table (if any)
  * @param memberMethods labels of all member methods
  * @param memberVars    symbols of all member variables
  */
class VTable(val name: String, val className: String, val parent: Option[VTable],
             val memberMethods: List[Label], val memberVars: List[MemberVarSymbol])