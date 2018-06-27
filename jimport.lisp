
(cl:in-package :abcl-cdk)

(defmacro jimport (java-package class &optional package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,(apply #'intern class
                          (when package (list package)))
       (concatenate 'string (symbol-name (quote ,java-package))
                    "."
                    (symbol-name (quote ,class))))))

(jimport |java.util| |Vector|)
(jimport |java.util| |Arrays|)
(jimport |java.lang| |Integer|)
(jimport |org.openscience.cdk| |CDKConstants|)
(jimport |org.openscience.cdk.interfaces| |IAtom|)
(jimport |org.openscience.cdk.interfaces| |IBond|)
(jimport |org.openscience.cdk.config| |Isotopes|)
(jimport |org.openscience.cdk.interfaces| |IPseudoAtom|)
(jimport |org.openscience.cdk| |AtomContainer|)
(jimport |org.openscience.cdk.graph| |ShortestPaths|)
(jimport |org.openscience.cdk.ringsearch| |AllRingsFinder|)
(jimport |org.openscience.cdk.tools.manipulator| |MolecularFormulaManipulator|)

(jimport |org.openscience.cdk.tools.manipulator| |MolecularFormulaManipulator|)
(jimport |org.openscience.cdk.tools.manipulator| |AtomContainerManipulator|)

(jimport |org.openscience.cdk.graph.invariant| |Canon|)
(jimport |org.openscience.cdk.graph| |GraphUtil|)

(jimport |javax.vecmath| |Point2d|)

(jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality|)
(jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality$Stereo|)
(jimport |org.openscience.cdk.interfaces| |IBond|)
(jimport |org.openscience.cdk.interfaces| |IBond$Stereo|)
(jimport |org.openscience.cdk.geometry| |GeometryTools|)

(jimport |org.openscience.cdk.inchi| |InChIGeneratorFactory|)

(jimport |org.openscience.cdk| |Reaction|)
(jimport |org.openscience.cdk| |ReactionSet|)

(jimport |java.awt| |Color|)
(jimport |java.awt| |Font|)
(jimport |java.awt| |Font$PLAIN|)
(jimport |java.lang| |String|)
(jimport |java.lang| |Double|)
(jimport |javax.vecmath| |Vector2d|)
(jimport |java.io| |ByteArrayOutputStream|)

(jimport |org.openscience.cdk.interfaces| |IAtom|)

(jimport |org.openscience.cdk.graph.invariant| |Canon|)
(jimport |org.openscience.cdk.graph| |GraphUtil|)
(jimport |org.openscience.cdk.tools.manipulator| |AtomTypeManipulator|)
(jimport |org.openscience.cdk.tools.manipulator| |AtomContainerManipulator|)

(jimport |org.openscience.cdk| |Atom|)
(jimport |org.openscience.cdk.graph| |ConnectivityChecker|)

(jimport |org.openscience.cdk.renderer| |AtomContainerRenderer|)
(jimport |org.openscience.cdk.renderer| |SymbolVisibility|)
(jimport |org.openscience.cdk.renderer.color| |CDK2DAtomColors|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$BackgroundColor|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$FitToScreen|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$BondLength|)

(jimport |org.openscience.cdk.renderer.generators.standard| |StandardGenerator|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardGenerator$AtomColor|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardGenerator$HashSpacing|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardGenerator$Visibility|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardGenerator$WaveSpacing|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardAtomGenerator|)
(jimport |org.openscience.cdk.renderer.generators.standard| |StandardBondGenerator|)


(jimport |org.openscience.cdk.renderer.font| |AWTFontManager|)
(jimport |org.freehep.graphicsio| |PageConstants|)
(jimport |org.freehep.graphicsio.svg| |SVGGraphics2D|)
(jimport |org.freehep.graphicsio.pdf| |PDFGraphics2D|)
(jimport |org.openscience.cdk.renderer.visitor| |AWTDrawVisitor|)
(jimport |org.openscience.cdk.layout| |StructureDiagramGenerator|)

(jimport |java.awt| |Dimension|)
(jimport |java.awt| |Rectangle|)
(jimport |java.awt.geom| |Rectangle2D$Double|)

(jimport |org.openscience.cdk| |Atom|)

(jimport |org.openscience.cdk.smiles| |SmilesParser|)
(jimport |org.openscience.cdk.smiles| |SmilesGenerator|)
(jimport |org.openscience.cdk| |DefaultChemObjectBuilder|)
(jimport |org.openscience.cdk.silent| |SilentChemObjectBuilder|)


