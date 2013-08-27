subroutine amumpu(option, type, kxmps, usersm, nprec,&
                  lresol, kvers)
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
! BUT : UTILITAIRE POUR LES TRAITEMENTS CONNEXES LORS DU LANCEMENT
!       DES DIFFERENTES ETAPES DE MUMPS.
!
! OPTION=1 GESTION DE LA STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
!       CETTE ROUTINE DOIT ETRE APPELLEE ENTRE LA PHASE D'ANALYSE ET
!       CELLE DE FACTORISATION NUMERIQUE
! OPTION=2 DETECTION DES SINGULARITES (APRES FACTO) ET STOCKAGE DE CES
!          INFOS DS L'OBJET JEVEUX '&&AMUMP.PIVNUL' (V V I DIM=N+2)
!
! OPTION=3 RECUPERE LE NUMERO DE VERSION (OCCURENCE MUMPS EXISTE DEJA)
! OPTION=31 IDEM MAIS ON CREE UNE OCCURENCE MUMPS TEMPORAIRE. OPERATION
!    UN PEU COUTEUSE A NE FAIRE QU'UNE FOIS PAR OPERATEUR(SD_SOLVEUR).
! DANS CES DEUX MODES, ON CONTROLE LE CARACTERE LICITE DU NUMERO DE
! VERSIONS: 4.9.2 OU 4.10.0 SINON UTMESS_F
!
! OPTION=4 RECUPERE LE DETERMINANT ET ON LE STOCKE DS L'OBJET JEVEUX
!          '&&AMUMP.DETERMINANT' (V V R DIM=3)
!
! IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS XMPS
!                       (INUTILE POUR OPTION=31)
! IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
!
! SI OPTION=1
! IN  USERSM :   K8   : STRATEGIE MEMOIRE DE L'UTILISATEUR
!                 (INFORMATION SOUVENT ISSUE DE SD_SOLVEUR.SLVK(8))
! SI OPTION=2
! IN  NPREC  :   IN   : NBRE DE DIGITS POUR DETECTION DE SINGULARITE
! IN LRESOL  :  LOG   : .TRUE. SI ON FAIT LE SOLVE, .FALSE. SINON
!
! SI OPTION=3 OU 31
! OUT KVERS  :  K24   : NUMERO DE VERSION DE MUMPS LICITE
!
! SI OPTION=4
! RAS
!---------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
#include "asterf.h"
#include "aster_types.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jjldyn.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utgtme.h"
#include "asterfort/wkvect.h"
#include "mumps/cmumps.h"
#include "mumps/dmumps.h"
#include "mumps/smumps.h"
#include "mumps/zmumps.h"
    integer :: option, kxmps, nprec
    character(len=1) :: type
    character(len=12) :: usersm
    character(len=24) :: kvers
    logical :: lresol
!
#ifdef _HAVE_MUMPS
#include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
    type (smumps_struc) , pointer :: smpsk
    type (cmumps_struc) , pointer :: cmpsk
    type (dmumps_struc) , pointer :: dmpsk
    type (zmumps_struc) , pointer :: zmpsk
    real(kind=8) :: rval(3), rval1, rval2, rval3, rval1b, rval2b, rval3b, rinf12
    real(kind=8) :: rinf13
    integer :: info16, info26, vali(10), icoefm, icn22, icn23, rang, n, iaux1
    integer ::  info3, nbproc, ifm, niv, ibid, ipiv, info28, info12, i
    integer ::  tmax, tmaxb, ltot, iret, isizemu, nsizemu, nsizema, execmu
    integer :: info34, icnt33
    integer :: pid
    mpi_int :: mpicou
    logical :: lpara, lpeak, lpb1
    character(len=2) :: fstring
    character(len=8) :: k8tab(3)
    character(len=10) :: strpid
    character(len=24) :: kpiv, valk(2), ksizemu
    character(len=80) :: nvers
!
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
    call infniv(ifm, niv)
!
!       ------------------------------------------------
! ---   INITS
!       ------------------------------------------------
    nvers(1:80)=''
    if (option .ne. 31) then
! --- OCCURENCE DE MUMPS EXISTE DEJA DS UN VECTEUR XMPS
        select case(type)
        case ('S')
        smpsk=>smps(kxmps)
        lpara=(smpsk%nprocs.gt.1)
        nbproc=smpsk%nprocs
        rang=smpsk%myid
        nvers=smpsk%version_number
        n=smpsk%n
        case ('C')
        cmpsk=>cmps(kxmps)
        lpara=(cmpsk%nprocs.gt.1)
        nbproc=cmpsk%nprocs
        rang=cmpsk%myid
        nvers=cmpsk%version_number
        n=cmpsk%n
        case ('D')
        dmpsk=>dmps(kxmps)
        lpara=(dmpsk%nprocs.gt.1)
        nbproc=dmpsk%nprocs
        rang=dmpsk%myid
        nvers=dmpsk%version_number
        n=dmpsk%n
        case ('Z')
        zmpsk=>zmps(kxmps)
        lpara=(zmpsk%nprocs.gt.1)
        nbproc=zmpsk%nprocs
        rang=zmpsk%myid
        nvers=zmpsk%version_number
        n=zmpsk%n
        case default
        ASSERT(.false.)
        end select
    else
! ---- ON CREE PUIS DETRUIT UNE OCCURENCE MUMPS TEMPORAIRE
        kxmps=1
        select case(type)
        case ('S')
        smpsk=>smps(kxmps)
        smpsk%comm=mpicou
        smpsk%sym=0
        smpsk%par=1
        smpsk%job=-1
        call smumps(smpsk)
        nvers=smpsk%version_number
        smpsk%job=-2
        call smumps(smpsk)
        case ('C')
        cmpsk=>cmps(kxmps)
        cmpsk%comm=mpicou
        cmpsk%sym=0
        cmpsk%par=1
        cmpsk%job=-1
        call cmumps(cmpsk)
        nvers=cmpsk%version_number
        cmpsk%job=-2
        call cmumps(cmpsk)
        case ('D')
        dmpsk=>dmps(kxmps)
        dmpsk%comm=mpicou
        dmpsk%sym=0
        dmpsk%par=1
        dmpsk%job=-1
        call dmumps(dmpsk)
        nvers=dmpsk%version_number
        dmpsk%job=-2
        call dmumps(dmpsk)
        case ('Z')
        zmpsk=>zmps(kxmps)
        zmpsk%comm=mpicou
        zmpsk%sym=0
        zmpsk%par=1
        zmpsk%job=-1
        call zmumps(zmpsk)
        nvers=zmpsk%version_number
        zmpsk%job=-2
        call zmumps(zmpsk)
        case default
        ASSERT(.false.)
        end select
    endif
!
!       ------------------------------------------------
! ---   GESTION STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
!       ------------------------------------------------
    if (option .eq. 1) then
!
! ---   INITS. PROPRE A L'OPTION
        select case(type)
        case ('S')
        info16=smpsk%infog(16)
        info26=smpsk%infog(26)
        info3=smpsk%infog(3)*4
        case ('C')
        info16=cmpsk%infog(16)
        info26=cmpsk%infog(26)
        info3=cmpsk%infog(3)*8
        case ('D')
        info16=dmpsk%infog(16)
        info26=dmpsk%infog(26)
        info3=dmpsk%infog(3)*8
        case ('Z')
        info16=zmpsk%infog(16)
        info26=zmpsk%infog(26)
        info3=zmpsk%infog(3)*16
        end select
        if (info3 .lt. 0) then
            info3=-info3/nbproc
        else
            info3=info3/(1024*1024*nbproc)
        endif
!
! ---   nsizema: TAILLE CUMULEE EN MO DES OBJETS MUMPS A,IRN,RHS..
! ---   EXECMU:  TAILLE EN MO DE L'EXECUTABLE MUMPS
        execmu=30
        ksizemu='&&TAILLE_OBJ_MUMPS'
        call jeveuo(ksizemu, 'L', isizemu)
        nsizema=-999
        do i = 1, nbproc
            nsizemu=zi(isizemu+i-1)
            if (nsizemu .gt. nsizema) nsizema=nsizemu
        enddo
!
! ---   MARGES POUR LES ESTIMATIONS (EN %) DE MUMPS IC ET OOC PLUS
! ---   CORRECTION POUR PRENDRE EN COMPTE MAX(A/IRN/JCN,PROC) ET
! ---   EXECUTABLE MUMPS.
        if (lpara) then
! ---     MARGE DU AU PARALLELISME
            icoefm=30
        else
! ---     MARGE DU AU SEQUENTIEL
            icoefm=10
        endif
! ---   MARGE POUR LES TRES PETITS CAS
        if (n .lt. 100) icoefm=50
!
! ---   CONSOS MUMPS IC ET OOC MAX SUR TOUS LES PROCS
        info16=int(info16*((icoefm+100)*1.d0/100.d0))+nsizema+execmu
        info26=int(info26*((icoefm+100)*1.d0/100.d0))+nsizema+execmu
! ---
! ---   TMAX: MAX DE LA RAM DISPO =
! ---       MEM_JOB - COURANT_JEVEUX - RELIQUAT (PYTHON, EXEC ASTER...)
! ---
        tmax=-999
        rval1=-999
        rval2=-999
        rval3=-999
        tmaxb=-999
        rval1b=-999
        rval2b=-999
        rval3b=-999
        icn22=-999
        icn23=-999
!
! ---   SI LA MESURE DE VMPEAK N'EST PAS DISPO, ON A MOINS CONFIANCE
! ---   DANS LA MESURE DE TMAX. DU COUP CERTAINS MSGS NE SONT PAS
! ---   AFFICHES.
! ---   SI GESTION_MEMOIRE='AUTO', ON BASCULE AUTOMATIQUEMENT
! ---   EN MODE OOC
! ---   SI GESTION_MEMOIRE='EVAL', ON ALERTE L'UTILISATEUR
!
        lpb1=.false.
        call utgtme(1, 'VMPEAK  ', rval, iret)
        if (rval(1) .le. 0) then
            lpeak=.false.
            if (usersm(1:4) .eq. 'AUTO') then
                lpb1=.true.
                usersm='OUT_OF_CORE'
                if (niv .ge. 2) call u2mess('I', 'FACTOR_82')
            endif
        else
            lpeak=.true.
        endif
        k8tab(1)='MEM_TOTA'
        k8tab(2)='COUR_JV '
        k8tab(3)='RLQ_MEM '
        call utgtme(3, k8tab, rval, iret)
        rval1=rval(1)
        rval2=rval(2)
        rval3=rval(3)
        if (iret .eq. 0) then
            tmax=int(rval1-(rval2+rval3))
        else
            ASSERT(.false.)
        endif
!
        select case(usersm)
        case('IN_CORE')
! --------------
! ---   IN-CORE
! --------------
        icn22=0
        icn23=0
        if ((tmax.lt.info16) .and. (lpeak)) then
            vali(1)=info16
            vali(2)=icoefm
            vali(3)=tmax
            call u2mesi('A', 'FACTOR_74', 3, vali)
        endif
        case ('OUT_OF_CORE')
! ------------------
! ---   OUT-OF-CORE
!-------------------
        icn22=1
        icn23=0
        if ((tmax.lt.info26) .and. (lpeak)) then
            vali(1)=info26
            vali(2)=icoefm
            vali(3)=tmax
            call u2mesi('A', 'FACTOR_75', 3, vali)
        endif
        case ('AUTO')
! -----------------------------------------------------------------
! ----- STRATEGIE DECIDEE EN FONCTION DES CAPACITES MACHINES ET DES
! ----- CONSOMMATIONS REQUISES PAR MUMPS
! -----------------------------------------------------------------
        if (tmax .ge. info16) then
            icn22=0
            icn23=max(int(0.95*tmax),info16)-(nsizema+execmu)
        else
            call jjldyn(0, -1, ltot)
            k8tab(1)='MEM_TOTA'
            k8tab(2)='COUR_JV'
            k8tab(3)='RLQ_MEM'
            call utgtme(3, k8tab, rval, iret)
            rval1b=rval(1)
            rval2b=rval(2)
            rval3b=rval(3)
            if (iret .eq. 0) then
                tmaxb=int(rval1b-(rval2b+rval3b))
            else
                ASSERT(.false.)
            endif
            if (niv .ge. 2) then
                vali(1)=int(rval1b-rval1)
                call u2mesi('I', 'FACTOR_51', 1, vali)
            endif
            if (tmaxb .ge. info16) then
                icn22=0
                icn23=max(int(0.95*tmaxb),info16)-(nsizema+execmu)
            else if ((tmaxb.ge.info26).and.(tmaxb.lt.info16)) then
                icn22=1
                icn23=max(int(0.95*tmaxb),info26)-(nsizema+execmu)
            else
                vali(1)=tmax
                vali(2)=tmaxb
                vali(3)=info16
                vali(4)=info26
                vali(5)=icoefm
                call u2mesi('F', 'FACTOR_76', 5, vali)
            endif
        endif
        case ('EVAL')
! --------------------------------------------------
! ---   OPTION DE PRE-EVALUATION DES CONSOS MEMOIRE
! --------------------------------------------------
        icn22=-1
        icn23=-1
        k8tab(1)='CUSE_JV'
        k8tab(2)='RLQ_MEM'
        call utgtme(2, k8tab, rval, iret)
        iaux1=int(rval(1)+rval(2))
        vali(1)=n
        vali(2)=max(iaux1,1)
        vali(3)=max(info16,1)
        vali(4)=max(info26,1)
        vali(5)=max(info3,1)
        vali(6)=vali(2)+vali(3)
        vali(7)=vali(2)+vali(4)
        call u2mesg('I', 'FACTOR_81', 0, valk, 7,&
                    vali, 0, 0.d0)
        if (.not.lpeak) call u2mess('A', 'FACTOR_83')
        case default
        ASSERT(.false.)
        end select
! --- CORRECTIF POUR BENEFICIER DES BOUCLES DE RATTRAPAGE SI VMPEAK
! --- NON EVALUABLE ET GESTION_MEMOIRE='AUTO'
        if (lpb1) usersm='AUTO'
!
! ---  MODIFICATION DU PARAMETRAGE MUMPS POUR LA SUITE DU PROCESSUS
! ---- (FACTORISATION NUMERIQUE + SOLVE)
        select case(type)
        case ('S')
        smpsk%icntl(22)=to_mumps_int(icn22)
        smpsk%icntl(23)=to_mumps_int(icn23)
        smpsk%ooc_tmpdir='.'
        case ('C')
        cmpsk%icntl(22)=to_mumps_int(icn22)
        cmpsk%icntl(23)=to_mumps_int(icn23)
        cmpsk%ooc_tmpdir='.'
        case ('D')
        dmpsk%icntl(22)=to_mumps_int(icn22)
        dmpsk%icntl(23)=to_mumps_int(icn23)
        dmpsk%ooc_tmpdir='.'
        case ('Z')
        zmpsk%icntl(22)=to_mumps_int(icn22)
        zmpsk%icntl(23)=to_mumps_int(icn23)
        zmpsk%ooc_tmpdir='.'
        end select
!
        if (niv .ge. 2) then
! ---  NIVEAU DEVELOPPEUR
! ---  AFFICHAGE DE CONTROLE POUR DIAGNOSTIC MEMOIRE FIN
! ---  RECUPERATION DE L'AFFICHAGE DES CONSOS SYSTEMES
! ---  (VMPEAK, VMSIZE, VMDATA) + FREE DS LE FICHIER FORT.11
! ---  SI ON DECOMMENTARISE LES LIGNES 'CALL SYSTEM()' + 'GETPID'
            pid=0
!          PID=getpid()
            if (abs(pid) < 10) then
                fstring = 'I1'
            else if (pid < 100) then
                fstring = 'I2'
            else if (pid < 1000) then
                fstring = 'I3'
            else if (pid < 10000) then
                fstring = 'I4'
            else if (pid < 100000) then
                fstring = 'I5'
            else if (pid < 1000000) then
                fstring = 'I6'
            else
                write(6,*)'READ_VMPEAK : PB FORMAT CHOICE !'
            endif
            write(strpid,'('//fstring//')')pid
!          str=""
!          str="/proc/"//trim(adjustl(strpid))//"/status"
!          CALL SYSTEM("cat "//str//" > fort.11")
!          CALL SYSTEM('free -m >> fort.11')
            write(ifm,*)
            write(ifm,*)'*********************************************'
            write(ifm,*)'<AMUMPU> GESTION MEMOIRE USERSM/ICN22/ICN23: ',&
     &      usersm,icn22,icn23
            write(ifm,*)'<AMUMPU> CONSO MUMPS EXEC/OBJET_AIRNJCN/IC/OOC ',&
     &                 execmu,nsizema,info16-(execmu+nsizema),&
     &                                info26-(execmu+nsizema)
            write(ifm,*)'<AMUMPU> 1ERE ESTIMATION JEVEUX/RELIQUAT/TMAX: ',&
     &                 rval2,rval3,tmax
            write(ifm,*)'<AMUMPU> 2NDE ESTIMATION JEVEUX/RELIQUAT/TMAX: ',&
     &                 rval2b,rval3b,tmaxb
            write(ifm,*)'*********************************************'
        endif
!
!       ------------------------------------------------
! ---   DETECTION DES SINGULARITES (APRES FACTO)
!       ------------------------------------------------
    else if (option.eq.2) then
!
! ---   INITS. PROPRE A L'OPTION
        select case(type)
        case ('S')
        info28=smpsk%infog(28)
        info12=smpsk%infog(12)
        case ('C')
        info28=cmpsk%infog(28)
        info12=cmpsk%infog(12)
        case ('D')
        info28=dmpsk%infog(28)
        info12=dmpsk%infog(12)
        case ('Z')
        info28=zmpsk%infog(28)
        info12=zmpsk%infog(12)
        end select
!
        if (nprec .ge. 0) then
            kpiv='&&AMUMP.PIVNUL'
            call jeexin(kpiv, ibid)
            if (ibid .ne. 0) then
                ASSERT(.false.)
            else
                call wkvect(kpiv, 'V V I', n+2, ipiv)
                if (lresol) then
! ---   KPIV(1)= NOMBRE DE PIVOTS QUASI NULS (TOUS LE PROCS)
                    if (info28 .gt. n) then
                        ASSERT(.false.)
                    else
                        zi(ipiv)=info28
                    endif
! ---   KPIV(2)= NOMBRE DE PIVOTS NEGATIFS (TOUS LE PROCS)
                    if (info12 .gt. n) then
                        ASSERT(.false.)
                    else
                        zi(ipiv+1)=info12
                    endif
                    if (rang .eq. 0) then
! ---   KPIV(3..) LES PIVOTS QUASI NULS (ONLY PROC 0)
                        select case(type)
                        case ('S')
                        do i = 1, info28
                            zi(ipiv+1+i)=smpsk%pivnul_list(i)
                        enddo
                        case ('C')
                        do i = 1, info28
                            zi(ipiv+1+i)=cmpsk%pivnul_list(i)
                        enddo
                        case ('D')
                        do i = 1, info28
                            zi(ipiv+1+i)=dmpsk%pivnul_list(i)
                        enddo
                        case ('Z')
                        do i = 1, info28
                            zi(ipiv+1+i)=zmpsk%pivnul_list(i)
                        enddo
                        end select
                    endif
! ---   BCAST POUR COMMUNIQUER L'INFO AUX AUTRES PROCS
                    call asmpi_comm_jev('BCAST', kpiv)
                endif
            endif
! ---  AFFICHAGE DE CONTROLE
            if (niv .ge. 2) then
                write(ifm,*)
                write(ifm,*)&
                '*********************************************'
                write(ifm,*)'<AMUMPU> TEST KPIV',zi(ipiv),zi(ipiv+1),&
                zi(ipiv+2)
                write(ifm,*)&
                '*********************************************'
            endif
!
        endif
!
!       ------------------------------------------------
! ---   NUMERO DE VERSION DE MUMPS (DETECTION ET CONTROLE)
!       ------------------------------------------------
    else if ((option.eq.3).or.(option.eq.31)) then
!
        kvers=''
        kvers=trim(adjustl(nvers))
        select case(kvers)
        case('4.9.2','4.10.0')
        case default
        call u2mesk('F', 'FACTOR_72', 1, kvers)
        end select
!
!       ------------------------------------------------
! ---   CALCUL DE DETERMINANT (APRES FACTO)
!       ------------------------------------------------
    else if (option.eq.4) then
!
! ---   INITS. PROPRE A L'OPTION
        select case(type)
        case ('S')
        rinf12=smpsk%rinfog(12)
        rinf13=smpsk%rinfog(13)
        info34=smpsk%infog(34)
        icnt33=smpsk%icntl(33)
        case ('C')
        rinf12=cmpsk%rinfog(12)
        rinf13=cmpsk%rinfog(13)
        info34=cmpsk%infog(34)
        icnt33=cmpsk%icntl(33)
        case ('D')
        rinf12=dmpsk%rinfog(12)
        rinf13=dmpsk%rinfog(13)
        info34=dmpsk%infog(34)
        icnt33=dmpsk%icntl(33)
        case ('Z')
        rinf12=zmpsk%rinfog(12)
        rinf13=zmpsk%rinfog(13)
        info34=zmpsk%infog(34)
        icnt33=zmpsk%icntl(33)
        end select
        if (icnt33 .eq. 1) then
            kpiv='&&AMUMP.DETERMINANT'
            call jeexin(kpiv, ibid)
            if (ibid .ne. 0) then
                call jeveuo(kpiv, 'E', ipiv)
            else
                call wkvect(kpiv, 'V V R', 3, ipiv)
            endif
! --- ON STOCKE LE CALCUL DU DET: MANTISSE * (2**EXP)
! --- MANTISSE=DCMPLX(RINF12,RINF13)
! --- EXP     =INFO34
            zr(ipiv) =rinf12
            zr(ipiv+1)=rinf13
            zr(ipiv+2)=info34
        endif
!
! --- CASE SUR LA VARIABLE OPTION
    else
        ASSERT(.false.)
    endif
!
    call jedema()
#endif
end subroutine
