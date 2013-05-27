        interface
          subroutine rcevfa(nommat,para,sm,cnoc,csno,csne,cspo,cspe,&
     &kemixt,cspto,cspte,cspmo,cspme,cfao,cfae)
            character(len=8) :: nommat
            real(kind=8) :: para(3)
            real(kind=8) :: sm
            character(len=24) :: cnoc
            character(len=24) :: csno
            character(len=24) :: csne
            character(len=24) :: cspo
            character(len=24) :: cspe
            logical :: kemixt
            character(len=24) :: cspto
            character(len=24) :: cspte
            character(len=24) :: cspmo
            character(len=24) :: cspme
            character(len=24) :: cfao
            character(len=24) :: cfae
          end subroutine rcevfa
        end interface
