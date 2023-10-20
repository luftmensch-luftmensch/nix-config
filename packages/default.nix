{pkgs ? null}: {
	# Httpie Desktop - cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.
	httpie-desktop = pkgs.callPackage ./httpie-desktop {};
}
