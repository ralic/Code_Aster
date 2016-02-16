subroutine prere1(solvez, base, iret, matpre, matass,&
                  npvneg, istop)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! BUT : FACTORISER UNE MATR_ASSE (LDLT/MULT_FRONT/MUMPS)
!       OU FABRIQUER UNE MATRICE DE PRECONDITIONNEMENT (GCPC)
!
! SOLVEZ (K19) IN : OBJET SOLVEUR (OU ' ')
! BASE (K1)    IN : BASE SUR LAQUELLE ON CREE LA MATRICE FACTORISEE
!                  (OU LA MATRICE DE PRECONDITIONNEMENT)
! IRET (I)     OUT : CODE_RETOUR :
!             /0 -> OK (PAR DEFAUT AVEC GCPC/PETSC)
!             /2 -> LA FACTORISATION N'A PAS PU SE FAIRE
!                   JUSQU'AU BOUT.
!             /1 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON A PERDU BEAUCOUP DE DECIMALES
!             /3 -> LA FACTORISATION EST ALLEE AU BOUT
!                   MAIS ON NE SAIT PAS DIRE SI ON A PERDU DES DECIMALES
!
! MATPRE(K19) IN/JXVAR : MATRICE DE PRECONDITIONNEMENT (SI GCPC)
! MATASS(K19) IN/JXVAR : MATRICE A FACTORISER OU A PRECONDITIONNER
! NPVNEG (I) OUT : NBRE DE TERMES DIAGONAUX NEGATIFS DE LA FACTORISEE
!          CE NBRE N'EST LICITE QUE SI LA MATRICE EST REELLE SYMETRIQUE
!          ET N'EST FOURNI QUE PAR UN SOLVEUR DIRECT: LDLT, MF OU MUMPS
! ISTOP (I)  IN: COMPORTEMENT EN CAS DE DETECTION DE SINGULARITE. CE
!                PARAMETRE N'A D'UTILITE QU'AVEC UN SOLVEUR DIRECT
!                  /0 -> SI IRET>0 : ERREUR <F>
!                  /1 -> SI IRET=1 : ALARME <A>
!                        SI IRET=2 : ERREUR <F>
!                  /2 -> LE PROGRAMME NE S'ARRETE PAS
!                        ET N'IMPRIME AUCUN MESSAGE.
!                 /-9999 -> ON PREND LA VALEUR PREVUE DS LA SD_SOLVEUR
!                        POUR STOP_SINGULIER (VALEUR 0 OU 1 SEULEMENT)
!                 /AUTRE --> ASSERT
!-----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterfort/apetsc.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedbg2.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pcldlt.h"
#include "asterfort/pcmump.h"
#include "asterfort/sdmpic.h"
#include "asterfort/tldlg3.h"
#include "asterfort/jelira.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
!
    integer :: npvneg, istop, iret
    character(len=1) :: base
    character(len=*) :: matass, matpre, solvez
!
    integer :: idbgav, ifm, niv, islvk, ibid
    integer :: islvi, lmat, nprec, ndeci, isingu, niremp
    integer :: istopz, iretgc, n1
    character(len=24) :: metres, precon
    character(len=19) :: matas, maprec, matas1, solveu
    character(len=8) :: renum, kmpic, kmatd, ksym
    character(len=24), pointer :: refa(:) => null()
    aster_logical :: dbg
!
!----------------------------------------------------------------------
    call jemarq()
    call jedbg2(idbgav, 0)
    call infniv(ifm, niv)

!     COHERENCE DES VALEURS DE ISTOPZ (NIVEAU DEVELOPPEUR)
    istopz=istop
    if ((istopz.ne.0) .and. (istopz.ne.1) .and. (istopz.ne.2) .and. (istopz.ne.-9999)) then
        ASSERT(.false.)
    endif
    dbg=.true.
    dbg=.false.

    matas1=matass
    matas = matass
    maprec = matpre
    npvneg=-9999

    solveu=solvez
    if (solveu .eq. ' ')  call dismoi('SOLVEUR', matas, 'MATR_ASSE', repk=solveu)
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    metres = zk24(islvk)

!   -- pour que la destruction des instances mumps et petsc fonctionne,
!      il faut renseigner le solveur dans la matrice :
    call jeveuo(matas//'.REFA', 'E', vk24=refa)
    refa(7)=solveu
!
    if (dbg) then
        call cheksd(matas, 'SD_MATR_ASSE', ibid)
        call cheksd(solveu, 'SD_SOLVEUR', ibid)
    endif

    call dismoi('MPI_COMPLET', matas, 'MATR_ASSE', repk=kmpic)
    call dismoi('MATR_DISTR', matas, 'MATR_ASSE', repk=kmatd)
    if ( niv == 2 ) then
        call dismoi('TYPE_MATRICE', matas, 'MATR_ASSE', repk=ksym)
        select case( ksym(1:7) ) 
        case( 'SYMETRI' )
            call utmess( 'I', 'ALGELINE5_2' )
        case( 'NON_SYM' )
            call utmess( 'I', 'ALGELINE5_3' )
        case default
        ASSERT(.false.)
        end select
    endif
    if (kmpic .eq. 'NON') then
        if (metres .eq. 'MUMPS' .or. ( metres.eq.'PETSC'.and.kmatd.eq.'OUI')) then
        else
            call sdmpic('MATR_ASSE', matas)
        endif
    endif
!
!
!
    call jeveuo(solveu//'.SLVI', 'L', islvi)
!
    call mtdscr(matas)
    call jeveuo(matas//'.&INT', 'E', lmat)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!             MULTIFRONTALE OU LDLT OU MUMPS               C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    if (metres .eq. 'LDLT' .or. metres .eq. 'MULT_FRONT' .or. metres .eq. 'MUMPS') then
        nprec = zi(islvi-1+1)
        if (istopz .eq. -9999) istopz = zi(islvi-1+3)
        renum=' '
        if (metres(1:10) .eq. 'MULT_FRONT') renum=zk24( islvk-1+4)(1:8)
        if ((metres(1:5).eq.'MUMPS') .and. (istopz.eq.2) .and. (nprec.lt.0)) then
            call utmess('F', 'ALGELINE5_74')
        endif
        call tldlg3(metres, renum, istopz, lmat, 1,&
                    0, nprec, ndeci, isingu, npvneg,&
                    iret, solveu)
        if ((nprec.lt.0) .and. (iret.ne.2)) iret=3
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                         PETSC                            C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    else if (metres.eq.'PETSC') then
        call apetsc('DETR_MAT', ' ', matas, [0.d0], ' ',&
                    0, ibid, iret)
        call apetsc('PRERES', solveu, matas, [0.d0], ' ',&
                    0, ibid, iret)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                         GCPC                             C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    else if (metres.eq.'GCPC') then
!
        call jeveuo(solveu//'.SLVK', 'L', islvk)
        call jeveuo(solveu//'.SLVI', 'E', islvi)
        precon=zk24(islvk-1+2)
        call jelira(matas//'.VALM', 'NUTIOC', n1)
        ASSERT(n1.eq.1 .or. n1.eq.2)
        if (n1.eq.2) call utmess('F', 'ASSEMBLA_1')
!
        if (precon .eq. 'LDLT_INC') then
            niremp = zi(islvi-1+4)
            call pcldlt(maprec, matas, niremp, base)
        else if (precon.eq.'LDLT_SP') then
            call pcmump(matas, solveu, iretgc)
            if (iretgc .ne. 0) then
                call utmess('F', 'ALGELINE5_76')
            endif
        endif
        iret=0
    endif
!
!
!
    call jedbg2(ibid, idbgav)
    call jedema()
end subroutine
