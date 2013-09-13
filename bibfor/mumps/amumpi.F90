subroutine amumpi(option, lquali, ldist, kxmps, type)
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
! BUT : ROUTINE DE PARAMETRAGE MUMPS POUR AMUMPS/C/D/Z
!
! IN  OPTION:   IN   : OPTION D'UTILISATION.
! IN  LQUALI:  LOG   : LOGICAL EN CAS DE CRITERE DE QUALITE
! IN  LDIST :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
! IN  KXMPS :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
! IN  TYPE  :   K1   : TYPE DU POINTEUR R OU C
!---------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
#include "aster_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/r4maem.h"
#include "asterfort/amumpu.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: kxmps, option
    logical :: lquali, ldist
    character(len=1) :: type
!
#ifdef _HAVE_MUMPS
#include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
    mpi_int :: mpicou, mpimum
    integer :: nicntl, ncntl
    parameter (nicntl=40,ncntl=15)
    type (smumps_struc) , pointer :: smpsk
    type (cmumps_struc) , pointer :: cmpsk
    type (dmumps_struc) , pointer :: dmpsk
    type (zmumps_struc) , pointer :: zmpsk
    integer :: ifm, niv, i, jrefa, isymm, jslvk, isymv, isym
    integer :: jslvi, nprec, ibid
    mumps_int :: i4, icntl(nicntl)
    real(kind=8) :: cntl(ncntl), rr4max
    logical :: lbid
    character(len=4) :: typm, etam
    character(len=12) :: k12bid
    character(len=14) :: nonu
    character(len=19) :: nomat, nosolv
    character(len=24) :: kvers
    call jemarq()
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
    call infniv(ifm, niv)
!
!       ------------------------------------------------
!        INITS
!       ------------------------------------------------
    rr4max=r4maem()
!
    if (type .eq. 'S') then
        smpsk=>smps(kxmps)
    else if (type.eq.'C') then
        cmpsk=>cmps(kxmps)
    else if (type.eq.'D') then
        dmpsk=>dmps(kxmps)
    else if (type.eq.'Z') then
        zmpsk=>zmps(kxmps)
    else
        ASSERT(.false.)
    endif
!
    nomat=nomats(kxmps)
    nosolv=nosols(kxmps)
    nonu=nonus(kxmps)
    etam=etams(kxmps)
    call jeveuo(nomat//'.REFA', 'L', jrefa)
    call jeveuo(nosolv//'.SLVK', 'L', jslvk)
    call jeveuo(nosolv//'.SLVI', 'L', jslvi)
    nprec=zi(jslvi)
!
!       -----------------------------------------------------
!        INITIALISATION SYM, PAR ET JOB POUR MUMPS (CREATION)
!       -----------------------------------------------------
    if (option .eq. 0) then
!
        if (type .eq. 'S') then
            smpsk%comm = mpicou
        else if (type.eq.'C') then
            cmpsk%comm = mpicou
        else if (type.eq.'D') then
            dmpsk%comm = mpicou
        else if (type.eq.'Z') then
            zmpsk%comm = mpicou
        else
            ASSERT(.false.)
        endif
!
! ---     ISYM = 0 => NON-SYMETRIQUE
! ---     ISYM = 1 => SYMETRIQUE DEFINIE POSITIVE
! ---     ISYM = 2 => SYMETRIQUE  GENERAL
! ---     ISYMM DEDUIT DE LA MATRICE : NONSYM OU SYMGEN
        typm=zk24(jrefa-1+9)(1:4)
        if (typm .eq. 'MR') then
            isymm=0
        else if (typm.eq.'MS') then
            isymm=2
        else
            ASSERT(.false.)
        endif
!
! ---     PRISE EN COMPTE DE LA VOLONTE DE L'UTILISATEUR
! ---     => ISYMV
        if (zk24(jslvk-1+3) .eq. 'NONSYM') then
            isymv=0
        else if (zk24(jslvk-1+3).eq.'SYMDEF') then
            isymv=1
        else if (zk24(jslvk-1+3).eq.'SYMGEN') then
            isymv=2
        else if (zk24(jslvk-1+3).eq.'AUTO') then
            isymv=-1
        else
            ASSERT(.false.)
        endif
!
! ---     STRATEGIE PRUDENTE ET CONSERVATIVE
! ---     SI AUTO: NONSYM OU SYMGEN SUIVANT LA STRUCTURE DE LA MATRICE
! ---     SINON, ON APPLIQUE LE CHOIX DE L'UTILISATEUR
        if (isymv .eq. -1) then
            isym=isymm
        else if (isymv.eq.0) then
            isym=isymv
        else
            if (isymm .eq. 0) then
                call utmess('F', 'FACTOR_56', sk=zk24(jslvk-1+3))
            else
                isym=isymv
            endif
        endif
!
! ---     PARAMETRES D'INITIALISATION DE L'OCCURENCE MUMPS KXMPS
        i4=to_mumps_int(isym)
        if (type .eq. 'S') then
            smpsk%sym = i4
            smpsk%par = 1
            smpsk%job = -1
        else if (type.eq.'C') then
            cmpsk%sym = i4
            cmpsk%par = 1
            cmpsk%job = -1
        else if (type.eq.'D') then
            dmpsk%sym = i4
            dmpsk%par = 1
            dmpsk%job = -1
        else if (type.eq.'Z') then
            zmpsk%sym = i4
            zmpsk%par = 1
            zmpsk%job = -1
        else
            ASSERT(.false.)
        endif
!
!       ------------------------------------------------------
!        INITIALISATION ICNTL/CNTL POUR MUMPS (ANALYSE +FACTO)
!       ------------------------------------------------------
    else if (option.eq.2) then
!
! ---     INIT
        do i = 1, nicntl
            icntl(i)=0
        enddo
        do i = 1, ncntl
            cntl(i)=0.d0
        enddo
!
! ---     TEST DE COMPATIBILITE DE LA VERSION DE MUMPS
        call amumpu(3, type, kxmps, k12bid, ibid,&
                    lbid, kvers)
!
! ---     MESSAGES/ALERTES MUMPS
        icntl(1) = to_mumps_int(ifm)
        icntl(2) = 0
        icntl(3) = 0
        icntl(4) = 1
        if (niv .ge. 2) then
! ---     ICNTL(4) = 1/ERROR MESSAGES ONLY 2/ERRORS, WARNINGS, 3 PUIS 4
            icntl(3) = to_mumps_int(ifm)
            icntl(4) = 2
        endif
! ---     FORMAT MATRICE
        icntl(5) = 0
! ---     PRETRAITEMENTS (SCALING/PERMUTATION)
        if (zk24(jslvk-1+2) .eq. 'SANS') then
            icntl(6) = 0
            icntl(8) = 0
            icntl(12) = 1
        else if (zk24(jslvk-1+2).eq.'AUTO') then
            icntl(6) = 7
            icntl(8) = 77
            icntl(12) = 0
        else
            ASSERT(.false.)
        endif
!
! ---     RENUMEROTATION
        if (zk24(jslvk-1+4) .eq. 'AMD') then
            icntl(7) = 0
        else if (zk24(jslvk-1+4).eq.'AMF') then
            icntl(7) = 2
        else if (zk24(jslvk-1+4).eq.'SCOTCH') then
            icntl(7) = 3
        else if (zk24(jslvk-1+4).eq.'PORD') then
            icntl(7) = 4
        else if (zk24(jslvk-1+4).eq.'METIS') then
            icntl(7) = 5
        else if (zk24(jslvk-1+4).eq.'QAMD') then
            icntl(7) = 6
        else if (zk24(jslvk-1+4).eq.'AUTO') then
            icntl(7) = 7
        else
            ASSERT(.false.)
        endif
!
! ---     INITIALISATION EN DUR (EN DOUBLONS VS CALL DMUMPS JOB=-1)
! ---     MAIS ON NE SAIT JAMAIS AVEC LES EVOLUTIONS DES INITS DU PACKAG
! ---     ET CELA PERMET DE SURCHARGER PLUS RAPIDEMENT POUR TESTER
!
! ---     TYPE DE RESOLUTION: A OU AT
        icntl(9) = 1
!
! ---     RAFFINEMENT ITERATIF ET ANALYSE QUALITE SOLUTION
! ---     PARAMETRES ACTIVES JUSTE AVANT SOLVE VIA AMUMPI OPTION=3
        icntl(10)=0
        cntl(2)=0.d0
        icntl(11)=0
!
! ---     PARALLELISME INDUIT PAR SCALAPACK (VOIR NPREC PLUS BAS)
        icntl(13) = 0
!
! ---     MEMOIRE SUPPL. POUR PIVOTAGE (DEFAUT:20)
        icntl(14) = to_mumps_int(zi(jslvi-1+2))
!
! ---     PAS UTILISES
        icntl(15)=0
        icntl(16)=0
        icntl(17)=0
!
! --      DETECTION DE SINGULARITE/NOYAU
        icntl(25)=0
        if (nprec .ge. 0) then
            icntl(13)=1
            icntl(24)=1
            cntl(3)=-10.d0**(-nprec)
            cntl(5)=1.d+6
        else
            icntl(24)=0
            cntl(3)=0.d0
            cntl(5)=0.d0
        endif
!
! ---     PIVOTAGE STATIQUE DESACTIVE
        cntl(4)=-1.d0
!
! ---     PARALLELISME/DISTRIBUTION SECOND MEMBRE/SOLUTION
        if (ldist) then
            icntl(18)=3
        else
            icntl(18)=0
        endif
        icntl(20)=0
        icntl(21)=0
!
! ---     GESTION MEMOIRE MUMPS
! ---     PARAMETRES ACTIVES APRES L'ANALYSE VIA AMUMPU OPTION=1
!
        icntl(22)=-999
        icntl(23)=-999
        if (type .eq. 'S') then
            smpsk%ooc_tmpdir='XXXX'
        else if (type.eq.'C') then
            cmpsk%ooc_tmpdir='XXXX'
        else if (type.eq.'D') then
            dmpsk%ooc_tmpdir='XXXX'
        else if (type.eq.'Z') then
            zmpsk%ooc_tmpdir='XXXX'
        else
            ASSERT(.false.)
        endif
!
! ---     COMPLEMENT DE SCHUR
        icntl(19)=0
        icntl(26)=0
!
! ---     PARAMETRE POUR RESOLUTIONS SIMULTANEES
        icntl(27)=-8
!
! ---     ANALYSE SEQUENTIELLE
        icntl(28)=0
        icntl(29)=0
!
! ---     PAS DE CALCUL DE TERMES DE A-1
        icntl(30)=0
!
! ---     ON GARDER LA FACTO EN MEMOIRE POUR LE SOLVE
        icntl(31)=0
!
! ---     NON UTILISE
        icntl(32)=0
!
! ---     PAS DE CALCUL DU DETERMINANT
        icntl(33)=0
!
! ---     NON UTILISES
        do i = 34, nicntl
            icntl(i)=0
        enddo
        do i = 6, ncntl
            cntl(i)=0.d0
        enddo
!
! ---   REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
! ---   DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
        if (type .eq. 'S') then
            do i = 1, nicntl
                smpsk%icntl(i)=icntl(i)
            enddo
            do i = 2, ncntl
                if (abs(cntl(i)) .gt. rr4max) ASSERT(.false.)
                smpsk%cntl(i)=real(cntl(i), kind=4)
            enddo
        else if (type.eq.'C') then
            do i = 1, nicntl
                cmpsk%icntl(i)=icntl(i)
            enddo
            do i = 2, ncntl
                if (abs(cntl(i)) .gt. rr4max) ASSERT(.false.)
                cmpsk%cntl(i)=real(cntl(i), kind=4)
            enddo
        else if (type.eq.'D') then
            do i = 1, nicntl
                dmpsk%icntl(i)=icntl(i)
            enddo
            do i = 2, ncntl
                dmpsk%cntl(i)=cntl(i)
            enddo
        else if (type.eq.'Z') then
            do i = 1, nicntl
                zmpsk%icntl(i)=icntl(i)
            enddo
            do i = 2, ncntl
                zmpsk%cntl(i)=cntl(i)
            enddo
        else
            ASSERT(.false.)
        endif
!
!       ------------------------------------------------------
!        INITIALISATION ICNTL/CNTL POUR MUMPS (SOLVE)
!       ------------------------------------------------------
    else if (option.eq.3) then
! ---   POUR CMD ECLATEE RESOUDRE PRINCIPALEMENT
! ---   TEST DU COMMUNICATEUR COURANT AU CAS OU (ERREUR PROGRAMMEUR).
! ---   IL DOIT ETRE IDENTIQUE A CELUI PARAMETRE DS L'OCCURENCE MUMPS
        if (type .eq. 'S') then
            mpimum=smpsk%comm
        else if (type.eq.'C') then
            mpimum=cmpsk%comm
        else if (type.eq.'D') then
            mpimum=dmpsk%comm
        else if (type.eq.'Z') then
            mpimum=zmpsk%comm
        else
            ASSERT(.false.)
        endif
        if (mpimum .ne. mpicou) ASSERT(.false.)
!
! ---     TEST DE COMPATIBILITE DE LA VERSION DE MUMPS
        call amumpu(3, type, kxmps, k12bid, ibid,&
                    lbid, kvers)
!
! ---     MESSAGE/ALERTES MUMPS
        icntl(1) = to_mumps_int(ifm)
        icntl(2) = 0
        icntl(3) = 0
        icntl(4) = 1
        if (niv .ge. 2) then
! ---     ICNTL(4) = 1/ERROR MESSAGES ONLY 2/ERRORS, WARNINGS, 3 PUIS 4
            icntl(3) = to_mumps_int(ifm)
            icntl(4) = 2
        endif
!
! ---     RAFFINEMENT ITERATIF ET ETUDE DE LA QUALITE
        icntl(10)=0
        icntl(11)=0
        cntl(2)=0.d0
        if (lquali) then
            if (zk24(jslvk-1+11) .eq. 'SANS') then
            else if (zk24(jslvk-1+11).eq.'AUTO') then
                icntl(10)=4
                cntl(2)=1.d-14
            else if (zk24(jslvk-1+11).eq.'FORCE') then
                icntl(10)=10
                cntl(2)=10.d-50
                if (type .eq. 'S' .or. type .eq. 'C') then
                    cntl(2)=1.d-38
                endif
            endif
            icntl(11)=1
        endif
        if (type .eq. 'S') then
            smpsk%icntl(1)=icntl(1)
            smpsk%icntl(2)=icntl(2)
            smpsk%icntl(3)=icntl(3)
            smpsk%icntl(4)=icntl(4)
            smpsk%icntl(10)=icntl(10)
            smpsk%icntl(11)=icntl(11)
            smpsk%cntl(2) =real(cntl(2), kind=4)
        else if (type.eq.'C') then
            cmpsk%icntl(1)=icntl(1)
            cmpsk%icntl(2)=icntl(2)
            cmpsk%icntl(3)=icntl(3)
            cmpsk%icntl(4)=icntl(4)
            cmpsk%icntl(10)=icntl(10)
            cmpsk%icntl(11)=icntl(11)
            cmpsk%cntl(2) =real(cntl(2), kind=4)
        else if (type.eq.'D') then
            dmpsk%icntl(1)=icntl(1)
            dmpsk%icntl(2)=icntl(2)
            dmpsk%icntl(3)=icntl(3)
            dmpsk%icntl(4)=icntl(4)
            dmpsk%icntl(10)=icntl(10)
            dmpsk%icntl(11)=icntl(11)
            dmpsk%cntl(2) =cntl(2)
        else if (type.eq.'Z') then
            zmpsk%icntl(1)=icntl(1)
            zmpsk%icntl(2)=icntl(2)
            zmpsk%icntl(3)=icntl(3)
            zmpsk%icntl(4)=icntl(4)
            zmpsk%icntl(10)=icntl(10)
            zmpsk%icntl(11)=icntl(11)
            zmpsk%cntl(2) =cntl(2)
        else
            ASSERT(.false.)
        endif
!
!       ------------------------------------------------
!        MAUVAISE OPTION
!       ------------------------------------------------
    else
        ASSERT(.false.)
    endif
    call jedema()
#endif
end subroutine
