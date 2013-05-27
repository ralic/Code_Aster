        interface
          subroutine rctype(jmat,nbpu,nompu,valpu,resu,type)
            integer :: jmat
            integer :: nbpu
            character(*) :: nompu(*)
            real(kind=8) :: valpu(*)
            real(kind=8) :: resu
            character(*) :: type
          end subroutine rctype
        end interface
