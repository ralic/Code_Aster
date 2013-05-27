        interface
          subroutine poeihm(nomte,option,modint,jgao,nno1,nno2,ncmp,&
     &nvim,vpg,vno)
            character(len=16) :: nomte
            character(len=16) :: option
            character(len=3) :: modint
            integer :: jgao
            integer :: nno1
            integer :: nno2
            integer :: ncmp
            integer :: nvim
            real(kind=8) :: vpg(*)
            real(kind=8) :: vno(*)
          end subroutine poeihm
        end interface
