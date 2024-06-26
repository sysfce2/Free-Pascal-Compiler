{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('nvapi');
    P.ShortName := 'nva';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'NVidia, Andreas Hausladen, Dmitry "skalogryz" Boyarintsev';
    P.License := 'NVidia license';
    P.HomepageURL := 'nvidia.com';
    P.Email := '';
    P.Description := 'NvAPI header';
    P.NeedLibC:= true;
    P.OSes:=[win32,win64];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('nvapi.pas');


    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
