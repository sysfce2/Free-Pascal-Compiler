{
    Copyright (c) 2024

    RISCV version of some node tree helper routines

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit nrvutil;

{$i fpcdefs.inc}

interface

  uses
    ngenutil;


  type
    trvnodeutils = class(tnodeutils)
      class procedure InsertObjectInfo; override;
    end;

implementation

  uses
    globtype,globals,
    systems,
    aasmdata,aasmtai;

  const
    tag_stack_align = 4;
    tag_arch = 5;
    tag_unaligned_access = 6;
    tag_priv_spec = 8;
    tag_priv_spec_minor = 10;
    tag_priv_spec_revision = 12;

  class procedure trvnodeutils.InsertObjectInfo;
    begin
      inherited InsertObjectInfo;
      if (target_info.system in systems_linux) then
        begin
          if (cs_create_pic in current_settings.moduleswitches) then
            current_asmdata.asmlists[al_start].Concat(tai_directive.create(asd_option,'pic'))
          else
            current_asmdata.asmlists[al_start].Concat(tai_directive.create(asd_option,'nopic'));

          current_asmdata.asmlists[al_start].Concat(tai_attribute.create(ait_attribute,tag_stack_align,target_info.stackalign));
          current_asmdata.asmlists[al_start].Concat(tai_attribute.create(ait_attribute,tag_unaligned_access,0));
        end;
    end;


begin
  cnodeutils:=trvnodeutils;
end.

