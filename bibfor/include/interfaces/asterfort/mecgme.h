        interface
          subroutine mecgme(modelz,carelz,mate,lischa,instap,depmoi,&
     &depdel,instam,compor,carcri,mesuiv)
            character(*) :: modelz
            character(*) :: carelz
            character(*) :: mate
            character(len=19) :: lischa
            real(kind=8) :: instap
            character(len=19) :: depmoi
            character(len=19) :: depdel
            real(kind=8) :: instam
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=19) :: mesuiv
          end subroutine mecgme
        end interface
