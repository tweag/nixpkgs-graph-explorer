def globals = [:]

globals << [g : traversal().withEmbedded(graph).withStrategies(ReferenceElementStrategy)]
