def globals = [:]

globals << [g : traversal().withEmbedded(graph).withStrategies(ReferenceElementStrategy)]

globals << [gReadOnly : traversal().withEmbedded(graph).withStrategies(ReadOnlyStrategy)]