module DigitalCupons.LocalSchema

open FSharp.Data

open FSharp.Data
open FSharp.Configuration

type Settings = AppSettings<"App.config">

[<Literal>]
let Localization = """<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:element name="localization">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="ciao"/>
            <xs:element type="xs:string" name="gestioneCorse"/>
            <xs:element type="xs:string" name="pickupDiscount"/>
            <xs:element type="xs:string" name="percentageDiscount"/>
            <xs:element type="xs:string" name="priceReduction"/>
            <xs:element type="xs:string" name="cuponForNumberOfPeople"/>
            <xs:element type="xs:string" name="claimThisCupon"/>
            <xs:element type="xs:string" name="cuponForm"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:schema>"""

type Resource = XmlProvider<Schema=Localization>