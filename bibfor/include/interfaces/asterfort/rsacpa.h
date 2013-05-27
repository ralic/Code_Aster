        interface
          subroutine rsacpa(nomsdz,numva,icode,nomva,ctype,ival,rval,&
     &kval,ier)
            character(*) :: nomsdz
            integer :: numva
            integer :: icode
            character(len=16) :: nomva
            integer :: ctype
            integer :: ival(*)
            real(kind=8) :: rval(*)
            character(len=80) :: kval(*)
            integer :: ier
          end subroutine rsacpa
        end interface
