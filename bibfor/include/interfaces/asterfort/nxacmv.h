        interface
          subroutine nxacmv(modele,mate,carele,fomult,charge,infcha,&
     &infoch,numedd,solveu,lostat,time,tpsthe,reasvc,reasvt,reasmt,&
     &reasrg,reasms,creas,vtemp,vhydr,tmpchi,tmpchf,vec2nd,vec2ni,matass&
     &,maprec,cndirp,cnchci,mediri,compor)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: fomult
            character(len=24) :: charge
            character(len=19) :: infcha
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=19) :: solveu
            logical :: lostat
            character(len=24) :: time
            real(kind=8) :: tpsthe(6)
            logical :: reasvc
            logical :: reasvt
            logical :: reasmt
            logical :: reasrg
            logical :: reasms
            character(len=1) :: creas
            character(len=24) :: vtemp
            character(len=24) :: vhydr
            character(len=24) :: tmpchi
            character(len=24) :: tmpchf
            character(len=24) :: vec2nd
            character(len=24) :: vec2ni
            character(len=24) :: matass
            character(len=19) :: maprec
            character(len=24) :: cndirp
            character(len=24) :: cnchci
            character(len=24) :: mediri
            character(len=24) :: compor
          end subroutine nxacmv
        end interface
