        interface
          subroutine vprecu(modes,nomsy,nbvect,lposi,nomvec,nbpara,&
     &nopara,nomvai,nomvar,nomvak,neq,nbmode,typmod,nbpari,nbparr,nbpark&
     &)
            character(*) :: modes
            character(*) :: nomsy
            integer :: nbvect
            integer :: lposi(*)
            character(*) :: nomvec
            integer :: nbpara
            character(*) :: nopara
            character(*) :: nomvai
            character(*) :: nomvar
            character(*) :: nomvak
            integer :: neq
            integer :: nbmode
            character(*) :: typmod
            integer :: nbpari
            integer :: nbparr
            integer :: nbpark
          end subroutine vprecu
        end interface
