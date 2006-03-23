Summary: Tools for using PXSL, the Parsimonious XML Shorthand Language
Name: pxsl-tools
Version: 0.9.3
Release: 1
License: GPL
Group: Development/Tools
URL: http://community.moertel.com/pxsl/
Source0: http://community.moertel.com/pxsl/pxsl-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildRequires: ghc >= 5.04.3, perl


%description
PXSL is a shorthand for XML that makes it easy for developers and
advanced content-creators to read, write, and edit markup-dense XML
documents.  It is especially well suited to handle data-centric uses
of XML and XML-embedded languages like XSLT.

%prep
%setup -q -n pxsl-%{version}

%build
make allopt

%install
rm -rf $RPM_BUILD_ROOT
libtool install -D -m 755 pxslcc $RPM_BUILD_ROOT%{_bindir}/pxslcc
libtool install -D -m 644 xsl2pxsl.xsl $RPM_BUILD_ROOT%{_datadir}/xsl2pxsl.xsl

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/pxslcc
%{_datadir}/xsl2pxsl.xsl
%doc README README.html TODO examples


%changelog
* Thu Jun  5 2003 Tom Moertel <tom@moertel.com>
- Initial build.


