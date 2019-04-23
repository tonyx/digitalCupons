module DigitalCupons.XmlProvider
open System.Net
open FSharp.Data

[<Literal>]
let autocompleteSchema = """<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="allowed">
    <xs:complexType>
      <xs:sequence>
        <xs:element type="xs:string" name="itemName" maxOccurs="unbounded" minOccurs="0"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>"""

type AutocompleteItems = XmlProvider<Schema=autocompleteSchema>