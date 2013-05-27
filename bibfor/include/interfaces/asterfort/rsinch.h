        interface
          subroutine rsinch(nomsd,nomch,acces,rval,chextr,proldr,&
     &prolga,istop,base,ier)
            character(*) :: nomsd
            character(*) :: nomch
            character(*) :: acces
            real(kind=8) :: rval
            character(*) :: chextr
            character(*) :: proldr
            character(*) :: prolga
            integer :: istop
            character(*) :: base
            integer :: ier
          end subroutine rsinch
        end interface
