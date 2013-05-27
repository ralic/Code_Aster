        interface
          subroutine tomabe(chmat,nmabet,nbmabe,mailla,nbnoma,mail2d,&
     &nbnobe,nunobe,xflu,xret,regl)
            character(len=8) :: chmat
            character(len=24) :: nmabet
            integer :: nbmabe
            character(len=8) :: mailla
            integer :: nbnoma
            logical :: mail2d
            integer :: nbnobe
            character(len=19) :: nunobe
            real(kind=8) :: xflu
            real(kind=8) :: xret
            character(len=4) :: regl
          end subroutine tomabe
        end interface
