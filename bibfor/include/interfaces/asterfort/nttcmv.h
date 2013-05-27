        interface
          subroutine nttcmv(modele,mate,carele,fomult,charge,infcha,&
     &infoch,numedd,solveu,time,chlapm,tpsthe,tpsnp1,reasvt,reasmt,creas&
     &,vtemp,vtempm,vec2nd,matass,maprec,cndirp,cnchci,cnchtp)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: fomult
            character(len=24) :: charge
            character(len=19) :: infcha
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=19) :: solveu
            character(len=24) :: time
            character(len=24) :: chlapm
            real(kind=8) :: tpsthe(6)
            real(kind=8) :: tpsnp1
            logical :: reasvt
            logical :: reasmt
            character(len=1) :: creas
            character(len=24) :: vtemp
            character(len=24) :: vtempm
            character(len=24) :: vec2nd
            character(len=24) :: matass
            character(len=19) :: maprec
            character(len=24) :: cndirp
            character(len=24) :: cnchci
            character(len=24) :: cnchtp
          end subroutine nttcmv
        end interface
