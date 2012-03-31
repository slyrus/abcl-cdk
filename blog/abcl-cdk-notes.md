# Cheminformatics, Java and, of Course, Common Lisp

_in which I attempt to write some Common Lisp code to be run in a
Common Lisp environment that runs inside a virtual machine designed to
support a C-like language that incorporated a few lispy features, so
that I can use a library written in said C-like language with my
Common Lisp code, or something like that._

Ok, it's time to see if I can get the
[CDK](http://sourceforge.net/apps/mediawiki/cdk/index.php?title=Main_Page)
and [ABCL](http://common-lisp.net/project/armedbear/) playing nicely
together.

## CDK

The
[CDK](http://sourceforge.net/apps/mediawiki/cdk/index.php?title=Main_Page)
(Chemistry Development Kit) is java library for dealing with various
type of chemistry data, elements, atoms, bonds, molecules, etc... and
various computed or measured properties thereof. I should point out
that the CDK isn't really just one library, but rather a family of
various related libraries. We'll come back to building an appropirate
version of CDK in a moment, but, for now, let's move on.

## ABCL

[ABCL](http://common-lisp.net/project/armedbear/) is an implementation
of the Common Lisp programming language that runs on the JVM. Besides
running (in theory) on any platform that supports the JVM, ABCL
provides for relatively smooth interoperability with other code (such
as Java libraries) that run on the JVM.

## Building CDK

First, we need the CDK. Some of the main things I want to do with the
CDK are to instantiate a molecule from a
[SMILES](http://www.daylight.com/dayhtml/doc/theory/theory.smiles.html)
string, get a 2D representation of the molecule, and compute various
properties (molecular weight, charge, etc...) of the molecule. The
only problem with that is the main CDK doesn't actually support 2D
rendering. Before we get into how to get a CDK that does 2D rendering,
I should take this opportunity to gripe about the various versions of
the CDK for a moment.

### Sourceforge's CDKs

One of the things that bothers me about sourceforge-hosted projects is
that there are often too many "home pages" for a project. For the CDK
we have two:


[http://sourceforge.net/projects/cdk/](http://sourceforge.net/projects/cdk/)

and

[http://cdk.sf.net](http://cdk.sf.net), which in turn redirects to:

[http://sourceforge.net/apps/mediawiki/cdk/index.php?title=Main_Page](http://sourceforge.net/apps/mediawiki/cdk/index.php?title=Main_Page)

Ok, so there's a bunch of info on the sf.net page and the [CDK
Development with
Git](http://sourceforge.net/apps/mediawiki/cdk/index.php?title=Development_with_Git)
page is kind enough to point us over to github:

     $ git clone git://github.com/cdk/cdk.git

which, of course implies that there is somthing of a home/project page
over at [https://github.com/cdk/cdk](https://github.com/cdk/cdk). And,
sure enough, there is.

From there we can see that github's cdk/cdk project is actually a fork
of [Egon Willighagen](https://github.com/egonw)'s [cdk git
repository](https://github.com/egonw/cdk).

### JChemPaint

Of course none of these (at least on first glance) contain the 2D
rendering code we want. It turns out that's not part of the core CDK,
but rather part of the JChemPaint code. The
[JChemPaint](http://sourceforge.net/apps/mediawiki/cdk/index.php?title=JChemPaint)
project is another effort, closely related to CDK, that has
applets/applications for interactive 2D molecule editing, 2D structure
rendering code, etc... So, on the JChemPaint page we see links to
various downloads where we have CDK, JChemPaint, CDK-JChemPaint,
etc...

Wait, what? CDK-JChemPaint? Hang on a second! We'll come back to that
in a moment. First we see that the CDK code is moving ahead rapidly
but that the JChemPaint is from September 2011 and the JChemPaint
(development) code is from November 2010! Hmm...

So, near as I can tell, JChemPaint was a separate, but related-to-CDK
project and at some point somebody cribbed some of the reusable bits
from JChemPaint and put them into CDK-JChemPaint.

But then it seems like maintaining a separate CDK-JChemPaint seemed a
bit silly and egonw (?) has been maintaining a branch of the CDK with
some of the JChemPaint (or is it CDK-JChemPaint?) functionality
incorporated:
[https://github.com/egonw/cdk/tree/13-unsorted-patches](https://github.com/egonw/cdk/tree/13-unsorted-patches). This
is what I orginally used for the 2D rendering code. It turns out that
there is a newer, better (?) version of the CDK with the appropriate
JChemPaint bits added, the 381-14x-renderextra branch.

### Back to building CDK...

First we get the code

    git clone git://github.com/cdk/cdk.git

Then we need to pull from egonw's branch (I suppose we could have just cloned this first):

    git remote add egonw git://github.com/egonw/cdk.git
    git pull egonw

And now let's checkout the branch we want:

    git checkout 381-14x-renderextra

Ok, now we've got the code. We build it with ant:

    ant

Assuming we have java properly setup, things should build fine. Now we
have a brazillion jar files in cdk/dist/jar. Wait, that's not what we
want. We want a single CDK jar that we can (presumaly) point our
CLASSPATH to, or at least do whatever the ABCL equivalent is. Turns
out there's a "dist-large" target in the CDK build.xml file so we can
build that with:

    ant dist-large

Ok, now we have dist/jar/cdk-1.4.8.git.jar.

## Installing CDK

So what are we supposed to do with that? Well, it appears that some
folks in the Java world use this thing called
[maven](http://maven.apache.org) for both remote and local package
fetching/deployment/whatever-you-call-it-in-the-java-world.

So, assuming we have maven around, we can install a CDK which we can
later, hopefully, use with ABCL with the following:

    export CDK_VERSION=1.4.8-SNAPSHOT
    export CDK_BUILD_VERSION=1.4.8.git
    mvn install:install-file -DgroupId=org.openscience.cdk -DartifactId=cdk \
        -Dversion=${CDK_VERSION} -Dpackaging=jar \
        -Dfile=dist/jar/cdk-${CDK_BUILD_VERSION}.jar

Notice that we need two distinct version identifiers as maven wants
nice clean version numbers (and doesn't really like the 1.4.8.git
version) and most maven-ized projects seem to use the SNAPSHOT suffix
for in-progress releases. On the other hand, the CDK build.props file
sets the version to 1.4.8.git. We use the two identifiers here so that
cdk-1.4.8.git.jar gits installed as org.openscience.cdk/cdk version
1.4.8-SNAPSHOT.

## Fetching Java Dependency Libraries

At this point I should point out that I'm not exactly a big maven
fan. It's no [quicklisp](http://www.quicklisp.org/). But there must be
a reason why folks in the Java world use it. Let's see what it takes
to download some more dependencies (presumably other jar files we're
going to use later). So, we fire off some queries on our favorite
search engine for, say, "maven fetch", and we see things like
[http://stackoverflow.com/questions/1895492/how-can-i-download-a-specific-maven-artifact-in-one-command-line](http://stackoverflow.com/questions/1895492/how-can-i-download-a-specific-maven-artifact-in-one-command-line)
and
[http://stackoverflow.com/questions/4568633/use-maven-just-to-fetch-some-library-jars](http://stackoverflow.com/questions/4568633/use-maven-just-to-fetch-some-library-jars). Oh,
man. I just want to download some jars and now I'm being told to use
Ivy (whatever that is) or some crazy maven plugin where all I need to
do is edit my ~/.m2/settings.xml and a ~/.m2/plugin-registry.xml file?
No thanks!

(Note: I think there's some built-in functionality in ABCL to handle
this next task -- but I couldn't get it to work!)

Fortunately, the clojure folks, who occasionally drink a little too
much Java toolchain (tooling?) Kool-aid for my taste, but at least
have enough taste to want a lisp-ish language, have gotten here first
and the standard tool for these kinds of jobs seems to be Phil
Hagelberg's [leiningen](https://github.com/technomancy/leiningen). I'm
going to assume for the moment that you actually have leningen lying
around, or that you're smart enough to figure out some other way to
get these dependencies installed if not.

So, to trick leiningen into doing some dirty work for us, we make a
project.clj that looks as follows:

    (defproject abcl-cdk-hacking "0.0.0"
      :description "Fake project for fetching abcl-cdk-hacking dependencies"
      :dependencies [[org.freehep/freehep-graphics2d "2.1.1"]
                     [org.freehep/freehep-graphicsio-pdf "2.1.1"]
                     [org.freehep/freehep-graphicsio-svg "2.1.1"]]
      :repositories {"freehep" "http://java.freehep.org/maven2"})

Once we have this we can do:
    
    lein deps

which will install the dependencies for us somewhere in ~/.m2 (let's
forget about system-wide installs for the moment).

## Using Java Dependency Libraries

Ok, we should be ready to figure out how to make ABCL talk to CDK
now. First we just have to figure out how to make ABCL talk to
CDK. Wait, wasn't that what I just said? Yes, but, how do we do it?
Fortunately, the ABCL guys anticpated this problem and added what they
call abcl-asdf. By doing a (require 'abcl-asdf) (oh wait, and a
(require 'abcl-contrib) before that, I think), we can tell our ASDF
system how to tell ABCL to tell the JVM where to find the jars we need
put on the CLASSPATH, or something like that.

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (cl:require 'abcl-contrib)
      (cl:require 'abcl-asdf))

    (asdf:defsystem :abcl-cdk-hacking
      :name "abcl-cdk-hacking"
      :author "Cyrus Harmon"
      :serial t
      :default-component-class asdf:cl-source-file
      :components
      ((:mvn "org.freehep/freehep-graphics2d" :version "2.1.1")
       (:mvn "org.freehep/freehep-graphicsio-pdf" :version "2.1.1")
       (:mvn "org.freehep/freehep-graphicsio-svg" :version "2.1.1")
       (:mvn "org.openscience/cdk" :version "1.4.8-SNAPSHOT")
       (:file "abcl-cdk-hacking")))

We can add :mvn components to our ASDF system and the abcl-asdf
machinery will add the maven artifact (?) or jar file or whatever to
the CLASSPATH, or at least somehow make it so the classes are
available to the JVM.

Well, that's the theory anyway. In practice this doesn't work with a
stock ABCL because of the following bug:
(http://trac.common-lisp.net/armedbear/ticket/204)[http://trac.common-lisp.net/armedbear/ticket/204]. Once
this is fixed (via the patch attached to the bug report), and ABCL
rebuilt, a simple:

    (asdf:load-system 'abcl-cdk-hacking)

will load the dependencies into the JVM and we should be off and running, finally.

## Calling Static Java Methods

Ok, now we need to do some Java interop stuff with CDK. First thing we
want to do is call a static Java method.

We're going to need an instance of the
org.openscience.cdk.DefaultChemObjectBuilder class. We can get this
via the static getInstance method as follows:

    (defparameter *dcob*
      (java:jcall
       (java:jmethod (java:jclass "org.openscience.cdk.DefaultChemObjectBuilder")
                     "getInstance")
       nil))

So, we have the java:jclass function to lookup a class, the
java:jmethod function to lookup a method and the java:jcall function
to invoke the method. So far so good.

## Creating Java Objects

Now we're going to need to create a Java object. Turns out we can do
that with the java:jnew function:

    (defparameter *smiles-parser*
      (java:jnew "org.openscience.cdk.smiles.SmilesParser" *dcob*))

This gives us a new instance of the
org.openscience.cdk.smiles.SmilesParser class.

## Calling Methods on Java Objects

Finally, we can call a java method with java:jcall, as we do with the
parseSmiles method here:

    (defparameter *caffeine*
      (java:jcall "parseSmiles" *smiles-parser* "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))

## Java Class Identifiers

Well, the clojure folks have figured out that some people, at least,
hate typing long java class names all over the place, and the ABCL
java interop stuff seems to require lots of typing of long java
names. In a perhaps misguided attempt to relieve this burden and
provide something more like clojure's syntax, I present the jimport
macro:

    (defmacro jimport (java-package class &optional package)
      `(defparameter ,(apply #'intern class
                             (when package (list package)))
         (concatenate 'string (symbol-name (quote ,java-package))
                      "."
                      (symbol-name (quote ,class)))))

This macro allows one to do:

    (jimport |org.openscience.cdk| |DefaultChemObjectBuilder|)

which then defines the value of the |DefaultChemObjectBuilder| symbol
(in the current packaage, at least if not specified in the jimport
call) to be "org.openscience.cdk.DefaultChemObjectBuilder", so now we
can do:

    (java:jcall
     (java:jmethod
      (java:jclass |DefaultChemObjectBuilder|) "getInstance")
     nil)

Not a huge win, but it does allow the compiler to ensure that we're
seeing identified symbols, rather than just potentially random strings
for Java classes.

## Java List<Foo>'s

One of the CDK classes,
org.openscience.cdk.renderer.AtomContainerRenderer, has a constructor
that expects a List<IGenerator<IAtomContainer>> as one of its
arguments. How do we invoke the constructor with one of those? Well,
it turns out we can't just use a lisp list as the argument. We have to
make a java List of some sort. It turns out there's some
infrastructure provided by ABCL to help with this, although nothing I
can find that does exactly what I need. The extensible-sequence stuff
allows us to make a lisp sequence that is actually some sort of
instance of the java.util.List interface. I use a java.util.Vector and
provide a helper function called jlist as follows:

    (defun jlist (&rest initial-contents)
      (sequence:make-sequence-like
       (java:jnew |Vector|) (length initial-contents)
       :initial-contents initial-contents))

So, now we've got a way to create java lists that we can pass on to
the constructor.

## Getting a java stream from a lisp stream

One final bit of consternation, we'd like to be able to create streams
using the sane lisp syntax like:

    (with-open-file (out-stream pathname :direction :output
                                           :if-exists :supersede
                                           :element-type :default)
      ...)

but then use the corresponding streams where we need java streams. In
particular the freehep SVG and PDF libraries want java streams for
files. It turns out there's a function to get the java output stream
associated with a lisp stream, getWrappedOutputStream. We use that to
get the java.io.Stream or whatever and we're good to go.

Now we can define our mol-to-svg function as follows:

    (defun mol-to-svg (mol pathname)
      (with-open-file (out-stream pathname :direction :output
                                           :if-exists :supersede
                                           :element-type :default)
        (let*
            ((r (java:jnew |AtomContainerRenderer|
                           (jlist
                            (java:jnew |BasicAtomGenerator|)
                            (java:jnew |BasicBondGenerator|)
                            (java:jnew |BasicSceneGenerator|))
                           (java:jnew |AWTFontManager|)))
             (vg (java:jnew |SVGGraphics2D|
                            (java:jcall "getWrappedOutputStream" out-stream)
                            (java:jnew |Dimension| 320 320)))
             (adv (java:jnew |AWTDrawVisitor| vg)))
          (java:jcall "startExport" vg)
          (java:jcall "generateCoordinates"
                      (java:jnew |StructureDiagramGenerator| mol))
          (java:jcall "setup" r mol (java:jnew |Rectangle| 0 0 100 100))
          (java:jcall "paint" r mol adv
                      (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                                 10 10 300 300)
                      java:+true+)
          (java:jcall "endExport" vg))))

Finally, we can render our molecule of choice, caffeine to an SVG file thusly:

    (mol-to-svg *caffeine* "/tmp/caffeine.svg")

And we see:

![caffeine SVG](/static/caffeine.svg)

Voila!

Next time hopefully we can explore integrating chemicl and CDK directly with ABCL, but I think that requires fixing an [ABCL bug](http://trac.common-lisp.net/armedbear/ticket/200) that prevents it from successfully compiling plexxipus-xpath.
