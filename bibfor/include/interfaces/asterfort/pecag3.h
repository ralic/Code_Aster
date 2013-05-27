        interface
          subroutine pecag3(ndim,nsymx,nsymy,noma,motcle,nbmail,noment&
     &,valpar)
            integer :: ndim
            logical :: nsymx
            logical :: nsymy
            character(*) :: noma
            character(*) :: motcle
            integer :: nbmail
            character(*) :: noment(*)
            real(kind=8) :: valpar(*)
          end subroutine pecag3
        end interface
