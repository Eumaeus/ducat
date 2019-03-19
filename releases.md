# Release notes

**0.9.5**: 6x speedup loading alignments.

**0.9.4**: Changed default for saving. Css.

**0.9.3**: Saving and loading CEX files.

**0.9.2**: UI work done for now. Grouping citable nodes according to penultimate level of citation scheme. Massive speedup on loading alignments. No longer throwing thousands of errors to the JS console.

**0.9.1**: Loading CEX, clicking to make alignments, navigating and seeing alignments, loading other texts based on one corpus' alignments all working.

**0.9.0**: Focusing down on alignment. Removed navigation buttons and functions (users can load texts from URNs). Removed the distinction between "new" and "saved" alignments.

**0.8.5**: Using OHCO2 10.12.2 for better performance on `validReff`. Removed namespaces on top-level JS libraries, to avoid deprecation warnings.

**0.8.4**: Removed debugging code. Build for Leiden demo.

**0.8.3**: Using `citealign` 0.4.1 for better performance.

**0.8.2**: Using `citealign` 0.4.0 for better performance.

**0.8.1**: Profiling for performance.

**0.8.0**: Collections correctly writing to CEX. Relations, too.

**0.7.0**: Exporting CEX with Collection definitions.

**0.6.3**: Exporting CEX with texts (all texts, shown texts, shown passages).

**0.6.2**: Improved README, better explanation. Now builds SPA.

**0.6.1**: Using `citealign` library 0.3.0. Added the foundation for exporting CEX files. Initial, sparsly populated "save dialog".

**0.6.0**: UI is pretty good for alignments, both those loaded from CEX and newly created. Allows deletion of newly-created, saved alignments.

**0.5.0**: Loads and display alignments in CEX files. First steps toward an integrated alignment-creating tool.

**0.4.1**: Updated Scala to 1.12.8 and ScalaJS-Dom to 0.9.6.

**0.3.0**: As a first step toward an alignment-reader, allow clickable highlighting of citable nodes. Fixed a bug with "all versions of text" when a text was pre-loaded from a request parameter.

**0.2.0**: Allow more than one view of the same text; accepts multiple CtsUrns as request-parameters with `?urn=…&urn=…`.

**0.1.0**: Feature complete for browsing.

**0.0.3**: Loading remote CEX on startup. Implemented "remove text" button for individual texts.

**0.0.2**: Loading CEX. Loading and displaying CEX texts. No navigation yet.

**0.0.1**: Initial build.
