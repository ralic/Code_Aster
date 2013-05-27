        interface
          subroutine rvcpnc(mcf,iocc,nch19,gd,typegd,nbcpc,nlscpc,&
     &nomojb,repere,option,quant,codir,dir,iret)
            character(*) :: mcf
            integer :: iocc
            character(len=19) :: nch19
            integer :: gd
            character(len=4) :: typegd
            integer :: nbcpc
            character(len=24) :: nlscpc
            character(len=24) :: nomojb
            character(len=8) :: repere
            character(len=16) :: option
            character(len=24) :: quant
            integer :: codir
            real(kind=8) :: dir(*)
            integer :: iret
          end subroutine rvcpnc
        end interface
