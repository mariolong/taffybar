{ mkDerivation, base, cairo, containers, dbus, dyre
, enclosed-exceptions, filepath, gtk, gtk-traymanager, gtk2
, HStringTemplate, HTTP, mtl, network, network-uri, old-locale
, parsec, process, safe, split, stdenv, stm, text, time
, time-locale-compat, transformers, utf8-string, X11, xdg-basedir
, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "taffybar";
  version = "0.4.6";
  sha256 = "1xfaw32yq17a6wm6gzvpdnpabxfnskwbs541h1kk1lvrkm31h2b2";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base cairo containers dbus dyre enclosed-exceptions filepath gtk
    gtk-traymanager HStringTemplate HTTP mtl network network-uri
    old-locale parsec process safe split stm text time
    time-locale-compat transformers utf8-string X11 xdg-basedir xmonad
    xmonad-contrib
  ];
  libraryPkgconfigDepends = [ gtk2 ];
  executableHaskellDepends = [
    base dyre filepath gtk safe xdg-basedir
  ];
  executablePkgconfigDepends = [ gtk2 ];
  homepage = "http://github.com/travitch/taffybar";
  description = "A desktop bar similar to xmobar, but with more GUI";
  license = stdenv.lib.licenses.bsd3;
}
