subroutine amumpt(option, kmonit, temps, rang, nbproc,&
                  kxmps, lquali, type, ietdeb, ietrat,&
                  rctdeb, ldist)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!--------------------------------------------------------------
! BUT : ROUTINE DE MONITORING POUR AMUMPS/C/D/Z.
!
! IN  OPTION :   IN   : OPTION D'UTILISATION.
! IN/OUT KMONIT: K24  : VECTEUR DE NOMS DES OBJ JEVEUX
! IN/OUT TEMPS : R8   : VECTEUR POUR UTTCPU
! IN     RANG  : IN   : RANG DU PROCESSEUR
! IN     NBPROC: IN   : NBRE DE PROCESSEURS
! IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
! IN  LQUALI :  LOG   : LOGICAL EN CAS DE CRITERE DE QUALITE
! IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
! IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
!---------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mpicm2.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
    integer :: option, rang, nbproc, kxmps, ietdeb, ietrat
    character(len=1) :: type
    character(len=24) :: kmonit(12)
    real(kind=8) :: temps(6), rctdeb
    logical :: lquali, ldist
!
#ifdef _HAVE_MUMPS
#include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
    type (smumps_struc) , pointer :: smpsk
    type (cmumps_struc) , pointer :: cmpsk
    type (dmumps_struc) , pointer :: dmpsk
    type (zmumps_struc) , pointer :: zmpsk
    integer :: ifm, niv, ibid, iaux1, iaux2, iaux3, k, i, n, info(100), iret
    integer :: monit(12), ietfin, ietmax, isizemu, execmu
    character(len=8) :: k8bid
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: ktemp
    character(len=24) :: ksizemu
    character(len=80) :: nvers
    real(kind=8) :: rmonit(18), rinfog(100), rctfin, retfin
    logical :: ldebug, lcmde
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- PARAMETRE POUR DEBUGGAGE
    ldebug=.true.
    ldebug=.false.
!
!       ------------------------------------------------
!        INITS
!       ------------------------------------------------
! --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
! --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
    if (type .eq. 'S') then
        smpsk=>smps(kxmps)
        n=smpsk%n
        info(9)=smpsk%info(9)
        info(15)=smpsk%info(15)
        info(16)=smpsk%info(16)
        info(17)=smpsk%info(17)
        rinfog(7)=smpsk%rinfog(7)
        rinfog(9)=smpsk%rinfog(9)
        rinfog(10)=smpsk%rinfog(10)
        nvers(1:15)='SMUMPS '//smpsk%version_number
    else if (type.eq.'C') then
        cmpsk=>cmps(kxmps)
        n=cmpsk%n
        info(9)=cmpsk%info(9)
        info(15)=cmpsk%info(15)
        info(16)=cmpsk%info(16)
        info(17)=cmpsk%info(17)
        rinfog(7)=cmpsk%rinfog(7)
        rinfog(9)=cmpsk%rinfog(9)
        rinfog(10)=cmpsk%rinfog(10)
        nvers(1:15)='CMUMPS '//cmpsk%version_number
    else if (type.eq.'D') then
        dmpsk=>dmps(kxmps)
        n=dmpsk%n
        info(9)=dmpsk%info(9)
        info(15)=dmpsk%info(15)
        info(16)=dmpsk%info(16)
        info(17)=dmpsk%info(17)
        rinfog(7)=dmpsk%rinfog(7)
        rinfog(9)=dmpsk%rinfog(9)
        rinfog(10)=dmpsk%rinfog(10)
        nvers(1:15)='DMUMPS '//dmpsk%version_number
    else if (type.eq.'Z') then
        zmpsk=>zmps(kxmps)
        n=zmpsk%n
        info(9)=zmpsk%info(9)
        info(15)=zmpsk%info(15)
        info(16)=zmpsk%info(16)
        info(17)=zmpsk%info(17)
        rinfog(7)=zmpsk%rinfog(7)
        rinfog(9)=zmpsk%rinfog(9)
        rinfog(10)=zmpsk%rinfog(10)
        nvers(1:15)='ZMUMPS '//zmpsk%version_number
    else
        call assert(.false.)
    endif
!
! --- TEST POUR LIMITER LE MONITORING DES CMDES ECLATEES
!     CAR LES OBJETS TEMPORAIRES DE MONITORING SONT EFFACES A CHAQUE
!     FIN DE COMMANDE (NUM_DDL/FACTORISER/RESOUDRE)
! --- ON N'AFFICHE LE MONITORING PROPRE A MUMPS QUE SI ON EST DS UNE
!     COMMANDE AGREGEE ET SI INFO=2
!
    call getres(k8bid, k16bid, nomcmd)
    if ((nomcmd(1:8).eq.'NUME_DDL') .or. (nomcmd(1:10).eq.'FACTORISER') .or.&
        (nomcmd(1:8).eq.'RESOUDRE')) then
        lcmde=.true.
    else
        lcmde=.false.
    endif
!
    if ((.not.lcmde) .and. (niv.ge.2)) then
! ---   VECTEURS DE MONITORING
        kmonit(1)='&MUMPS.INFO.MAILLE'
        kmonit(2)='&MUMPS.INFO.MEMOIRE'
        kmonit(9)='&MUMPS.NB.MAILLE'
        kmonit(10)='&MUMPS.INFO.MEM.EIC'
        kmonit(11)='&MUMPS.INFO.MEM.EOC'
        kmonit(12)='&MUMPS.INFO.MEM.USE'
!
! ---   TEST POUR EVITER LE MONITORING LORSQUE LDLT_SP EST UTILISE
        call jeexin(kmonit(1), iret)
        if (iret .eq. 0) goto 999
!
        call jeveuo(kmonit(1), 'E', monit(1))
        call jeveuo(kmonit(2), 'E', monit(2))
        call jeveuo(kmonit(9), 'E', monit(9))
        call jeveuo(kmonit(10), 'E', monit(10))
        call jeveuo(kmonit(11), 'E', monit(11))
        call jeveuo(kmonit(12), 'E', monit(12))
    endif
!       ------------------------------------------------
!       TRAITEMENTS PROPREMENT DIT
!       ------------------------------------------------
    if (option .eq. 0) then
! --    ON NE FAIT RIEN DE PLUS !
    else if (option.eq.1) then
        if (ldebug) then
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
!
    else if (option.eq.2) then
        call uttcpu('CPU.AMUMPT', 'INIT ', ' ')
        call uttcpu('CPU.AMUMPT', 'DEBUT', ' ')
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'REMPLISSAGE MATRICE ' //'TEMPS CPU/ELAPSED ',&
            rctfin-rctdeb,retfin
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
!       -- ON INTERROMPT LA MESURE CPU.RESO.4 PENDANT CPU.RESO.3 :
        call uttcpu('CPU.RESO.4', 'FIN', ' ')
        call uttcpu('CPU.RESO.3', 'DEBUT', ' ')
!
    else if (option.eq.4) then
        call uttcpu('CPU.RESO.3', 'FIN', ' ')
        call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
        call uttcpu('CPU.AMUMPT', 'FIN', ' ')
        call uttcpr('CPU.AMUMPT', 6, temps)
        if ((.not.lcmde) .and. (niv.ge.2)) then
            zi(monit(10)+rang)=info(15)
            zi(monit(11)+rang)=info(17)
        endif
        call uttcpu('CPU.AMUMPT', 'INIT ', ' ')
        call uttcpu('CPU.AMUMPT', 'DEBUT', ' ')
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'ANALYSE MUMPS ' //'TEMPS CPU/ELAPSED ',&
            rctfin-rctdeb,retfin
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
!
    else if (option.eq.6) then
        call uttcpu('CPU.AMUMPT', 'FIN', ' ')
        call uttcpr('CPU.AMUMPT', 6, temps)
        if ((.not.lcmde) .and. (niv.ge.2)) then
            zi(monit(2)+rang)=info(9)
            zi(monit(12)+rang)=info(16)
            call mpicm2('REDUCE', kmonit(1))
            call mpicm2('REDUCE', kmonit(2))
        endif
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'FACTO NUMERIQUE MUMPS '&
     &          //'TEMPS CPU/ELAPSED ',rctfin-rctdeb,retfin
        endif
!
    else if (option.eq.7) then
        if (ldebug) then
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
    else if (option.eq.8) then
        call uttcpu('CPU.AMUMPT', 'INIT ', ' ')
        call uttcpu('CPU.AMUMPT', 'DEBUT', ' ')
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'PRETRAITEMENTS RHS ' //'TEMPS CPU/ELAPSED ',&
            rctfin-rctdeb,retfin
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
!
    else if (option.eq.10) then
        call uttcpu('CPU.AMUMPT', 'FIN', ' ')
        call uttcpr('CPU.AMUMPT', 6, temps)
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'DESCENTE-REMONTEE MUMPS '&
     &          //'TEMPS CPU/ELAPSED ',rctfin-rctdeb,retfin
            call system_clock(ietdeb, ietrat, ietmax)
            call cpu_time(rctdeb)
        endif
!
    else if (option.eq.12) then
        if (ldebug) then
            call system_clock(ietfin)
            retfin=real(ietfin-ietdeb)/real(ietrat)
            call cpu_time(rctfin)
            write(ifm,*)'POST-TRAITEMENTS SOLUTION '&
     &          //'TEMPS CPU/ELAPSED ',rctfin-rctdeb,retfin
        endif
        if ((niv.ge.2) .and. (.not.lcmde)) then
! -- COMMUNICATION DES DONNEES DU MONITORING
            call mpicm2('REDUCE', kmonit(10))
            call mpicm2('REDUCE', kmonit(11))
            call mpicm2('REDUCE', kmonit(12))
! -- AFFICHAGE
            if (rang .eq. 0) then
                write(ifm,*)
                write(ifm,*)&
                '*********************************************'&
                //'*********************************'
                if (ldist) then
                    write(ifm,*)' CALCUL MUMPS DISTRIBUE'
                else
                    write(ifm,*)' CALCUL MUMPS CENTRALISE'
                endif
                write(ifm,*)'<MONITORING '//nvers(1:15)//' >'
                write(ifm,'(A19,I9)')' TAILLE DU SYSTEME ',n
                if (lquali) then
                    write(ifm,'(A28,1PD11.4,1PD11.4)')&
                    'CONDITIONNEMENT'// '/ERREUR ALGO ',rinfog(10),&
                    rinfog(7)
                    write(ifm,'(A23,1PD11.4)')'ERREUR SUR LA SOLUTION ',&
     &           rinfog(9)
                endif
                iaux1=0
                iaux2=0
                iaux3=0
                write(ifm,*)'RANG    '//' NBRE MAILLES    '//&
     &         ' NBRE TERMES K    '//' LU FACTEURS'
                do k = 0, nbproc-1
                    write(ifm,1001)k,zi(monit(9)+k),zi(monit(1)+k),&
                zi(monit(2)+k)
                    iaux1=iaux1+zi(monit(9)+k)
                    iaux2=iaux2+zi(monit(1)+k)
                    iaux3=iaux3+zi(monit(2)+k)
                enddo
! -- EN CENTRALISE ON NE FAIT PAS LA SOMME
                if (.not.ldist) iaux1=iaux1/nbproc
!
                write(ifm,*)'--------------------------------------------'&
     &          //'---------------'
                write(ifm,1003)iaux1,iaux2,iaux3
                write(ifm,*)
                do i = 1, 18
                    rmonit(i)=0.d0
                enddo
!
! -- MONITORING MEMOIRE
! ---   ZI(isizemu+RANG-1): TAILLE CUMULEE EN MO OBJETS MUMPS A,IRN...
! ---   EXECMU:  TAILLE EN MO DE L'EXECUTABLE MUMPS
                execmu=30
                KSIZEMU='&&TAILLE_OBJ_MUMPS'
                call jeveuo(KSIZEMU, 'L', isizemu)
                KSIZEMU='&&TAILLE_OBJ_MUMPS'
                if (type .eq. 'S') then
                    ibid=smpsk%icntl(22)
                else if (type.eq.'C') then
                    ibid=cmpsk%icntl(22)
                else if (type.eq.'D') then
                    ibid=dmpsk%icntl(22)
                else if (type.eq.'Z') then
                    ibid=zmpsk%icntl(22)
                else
                    call assert(.false.)
                endif
                if (ibid .eq. 0) then
                    ktemp='IN-CORE'
                else
                    ktemp='OUT-OF-CORE'
                endif
                write(ifm,*)' MEMOIRE RAM ESTIMEE ET REQUISE&
     &             EN MO(OBJETS MUMPS + EXECUTABLE)'
!
                write(ifm,*)'RANG ASTER : '//&
     &            'ESTIM IN-CORE | ESTIM OUT-OF-CORE | RESOL. '//ktemp
!
!           REAJUSTEMENT POUR TENIR COMPTE DE LA TAILLE DE L'EXECUTABLE
!           ET DES OBJETS PRE-ALLOUES AVANT L'ANALYSE
                do k = 0, nbproc-1
                    zi(monit(10)+k)=zi(monit(10)+k)+execmu+zi(isizemu+k)
                    zi(monit(11)+k)=zi(monit(11)+k)+execmu+zi(isizemu+k)
                    zi(monit(12)+k)=zi(monit(12)+k)+execmu+zi(isizemu+k)
                enddo
!           POUR LE CALCUL DES MOYENNES
                rmonit(1)=0.0
                rmonit(2)=0.0
                rmonit(3)=0.0
!           POUR LE CALCUL DES MAX
                rmonit(4)=zi(monit(10))
                rmonit(5)=zi(monit(11))
                rmonit(6)=zi(monit(12))
!           POUR LES CALCULS DES MIN
                rmonit(7)=zi(monit(10))
                rmonit(8)=zi(monit(11))
                rmonit(9)=zi(monit(12))
                do k = 0, nbproc-1
                    write(ifm,1001) k,zi(monit(10)+k),zi(monit(11)+k),&
                zi(monit(12)+k)
                    rmonit(1)=rmonit(1)+zi(monit(10)+k)
                    rmonit(2)=rmonit(2)+zi(monit(11)+k)
                    rmonit(3)=rmonit(3)+zi(monit(12)+k)
                    if (rmonit(4) .gt. zi(monit(10)+k)) rmonit(4)=zi( monit(10)+k)
                    if (rmonit(5) .gt. zi(monit(11)+k)) rmonit(5)=zi( monit(11)+k)
                    if (rmonit(6) .gt. zi(monit(12)+k)) rmonit(6)=zi( monit(12)+k)
                    if (rmonit(7) .lt. zi(monit(10)+k)) rmonit(7)=zi( monit(10)+k)
                    if (rmonit(8) .lt. zi(monit(11)+k)) rmonit(8)=zi( monit(11)+k)
                    if (rmonit(9) .lt. zi(monit(12)+k)) rmonit(9)=zi( monit(12)+k)
!
                enddo
                rmonit(1)=rmonit(1)/nbproc
                rmonit(2)=rmonit(2)/nbproc
                rmonit(3)=rmonit(3)/nbproc
                write(ifm,*)'------------------------------------------'//&
     &            '-----------------------------------'
                write(ifm,1007) rmonit(1),rmonit(2),rmonit(3)
                write(ifm,*)'------------------------------------------'//&
     &            '-----------------------------------'
!
                write(ifm,1008) rmonit(4),rmonit(5),rmonit(6)
                write(ifm,*)'------------------------------------------'//&
     &            '-----------------------------------'
!
                write(ifm,1009) rmonit(7),rmonit(8),rmonit(9)
                write(ifm,*)'------------------------------------------'//&
     &            '-----------------------------------'
!
!
                write(ifm,*)&
                '*********************************************'&
                //'*********************************'
                1001      format(' N ',i4,' :    ',i12,'    ',i12,'    ',i12)
                1002      format(' N ',i4,' :',1pd10.2,' ',1pd10.2,' ',1pd10.2,7x,&
     &              1pd10.2,' ',1pd10.2,' ',1pd10.2)
                1003      format('TOTAL   : ',i15,' ',i15,' ',i15)
                1004      format('MOYENNE :',1pd10.2,' ',1pd10.2,' ',1pd10.2,7x,&
     &              1pd10.2,' ',1pd10.2,' ',1pd10.2)
                1005      format('        :',1pd10.2,' ',1pd10.2,' ',1pd10.2,7x,&
     &              1pd10.2,' ',1pd10.2,' ',1pd10.2)
                1006      format('        :',1pd10.2,' ',1pd10.2,' ',1pd10.2,7x,&
     &              1pd10.2,' ',1pd10.2,' ',1pd10.2)
!
                1007      format('MOYENNE :      ',1pd10.2,'      ',1pd10.2,'      ',&
     &              1pd10.2)
                1008      format('MINIMUM :      ',1pd10.2,'      ',1pd10.2,'      ',&
     &              1pd10.2)
                1009      format('MAXIMUM :      ',1pd10.2,'      ',1pd10.2,'      ',&
     &              1pd10.2)
! FIN DU IF RANG
            endif
!
            do i = 1, nbproc
                zi(monit(1)+i-1)=0
                zi(monit(2)+i-1)=0
                zi(monit(10)+i-1)=0
                zi(monit(11)+i-1)=0
                zi(monit(12)+i-1)=0
            enddo
! FIN DU IF NIV + LCMDE
        endif
    else
! --- OPTION IMPREVUE
        call assert(.false.)
!
    endif
    999 call jedema()
#endif
end subroutine
