Name:           decanter
Version:        0.1.0.0
Release:        1%{?dist}
Summary:        Wine bottle manager with BTRFS snapshots

# Passe die Lizenz an dein Projekt an (z.B. MIT, BSD, GPLv3)
License:        GPL-v3
URL:            https://github.com/Borgvall/Decanter
# ÄNDERUNG: Volle URL, damit Copr/rpmbuild den Sourceball finden/laden kann
Source0:        https://github.com/Borgvall/Decanter/archive/refs/tags/v%{version}.tar.gz

# Build-Abhängigkeiten
BuildRequires:  ghc
BuildRequires:  cabal-install
BuildRequires:  gtk4-devel
BuildRequires:  libadwaita-devel
# Falls linux-btrfs C-Header braucht:
BuildRequires:  kernel-headers 

# Laufzeit-Abhängigkeiten (aus Logic.hs abgeleitet)
Requires:       wine
Requires:       winetricks
Requires:       xdg-utils
# Um sicherzustellen, dass BTRFS-Tools vorhanden sind (guter Stil)
Requires:       btrfs-progs

%description
Decanter is a GTK4/Adwaita application for managing Wine bottles.
It utilizes BTRFS snapshots for versioning and restoration, and supports
running Windows software via Wine.

%prep
# ÄNDERUNG: Entpackt den Quellcode. 
# Da GitHub-Archive oft Ordnernamen wie "Decanter-0.1.0.0" oder "Decanter-v..." haben, 
# nutzen wir -n, falls der Ordnername vom Standard abweicht. 
# Wenn du kein Tag v0.1.0.0 hast, könnte dieser Schritt fehlschlagen (siehe unten).
%setup -q

%build
# HINWEIS: Offizielle Paketierung erlaubt oft keinen Netzwerkzugriff im Build.
# Für private Builds ist 'cabal update' hier okay. Für offizielle Repos
# müsstest du 'cabal-rpm' nutzen oder Abhängigkeiten 'vendoren'.
cabal update
cabal build --enable-relocatable --disable-tests

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{_bindir}

# Wir suchen das kompilierte Binary in dist-newstyle und kopieren es nach /usr/bin
# Der Pfad ist dynamisch, daher 'find'
find dist-newstyle -name decanter -type f -perm -755 -exec cp {} %{buildroot}%{_bindir}/decanter \;

# --- NEU: Desktop-Datei und Icon installieren ---
mkdir -p %{buildroot}%{_datadir}/applications
cp data/com.github.borgvall.decanter.desktop %{buildroot}%{_datadir}/applications/

mkdir -p %{buildroot}%{_datadir}/icons/hicolor/scalable/apps/
cp data/com.github.borgvall.decanter.svg %{buildroot}%{_datadir}/icons/hicolor/scalable/apps/

%files
%doc README.md
%{_bindir}/decanter
%{_datadir}/applications/com.github.borgvall.decanter.desktop
%{_datadir}/icons/hicolor/scalable/apps/com.github.borgvall.decanter.svg

# ÄNDERUNG: Changelog hinzugefügt (Zwingend erforderlich für RPM Builds)
%changelog
* Wed Dec 17 2024 Johannes Roehl <dev@example.com> - 0.1.0.0-1
- Initial package release
