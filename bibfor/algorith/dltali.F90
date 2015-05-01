subroutine dltali(neq, result, imat, masse, rigid,&
                  liad, lifo, nchar, nveca, lcrea,&
                  lprem, lamort, t0, mate, carele,&
                  charge, infoch, fomult, modele, numedd,&
                  nume, solveu, criter, dep0, vit0,&
                  acc0, fexte0, famor0, fliai0, baseno,&
                  tabwk, force0, force1)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
!       DYNAMIQUE LINEAIRE TRANSITOIRE - ALGORITHME - INITIALISATION
!       -         -        -             --           -
! ----------------------------------------------------------------------
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : IMAT      : TABLEAU D'ADRESSES POUR LES MATRICES
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : RIGID     : MATRICE DE RIGIDITE
!  IN  : LIAD      : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
!  IN  : LIFO      : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
!  IN  : NCHAR     : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
!  IN  : NVECA     : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
!  IN  : LCREA     : LOGIQUE INDIQUANT SI IL Y A REPRISE
!  IN  : LAMORT    : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
!  IN  : MATE      : NOM DU CHAMP DE MATERIAU
!  IN  : CARELE    : CARACTERISTIQUES DES POUTRES ET COQUES
!  IN  : CHARGE    : LISTE DES CHARGES
!  IN  : INFOCH    : INFO SUR LES CHARGES
!  IN  : FOMULT    : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
!  IN  : MODELE    : MODELE
!  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
!  IN  : NUME      : NUMERO D'ORDRE DE REPRISE
!  IN  : SOLVEU    : NOM DU SOLVEUR
!  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
!  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
!  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
!  IN  : BASENO    : BASE DES NOMS DE STRUCTURES
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ajlagr.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dlfdyn.h"
#include "asterfort/dlfext.h"
#include "asterfort/dltini.h"
#include "asterfort/getvid.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: neq
    integer :: nveca, nchar
    integer :: liad(*)
    integer :: imat(3), nume
!
    real(kind=8) :: dep0(*), vit0(*), acc0(*)
    real(kind=8) :: fexte0(*), famor0(*), fliai0(*)
    real(kind=8) :: t0
    real(kind=8) :: tabwk(*)
!
    character(len=8) :: baseno, result
    character(len=8) :: masse, rigid
    character(len=19) :: solveu
    character(len=24) :: charge, infoch, fomult, mate, carele
    character(len=24) :: modele, numedd
    character(len=24) :: lifo(*)
    character(len=24) :: criter
    character(len=19) :: force0, force1
!
    aster_logical :: lcrea, lprem
    aster_logical :: lamort
!
    complex(kind=8) :: cbid
!
    integer :: inchac
!
    integer :: ibid, icode, ieq, ndy, ifextm, ifextc
    character(len=8) :: matrei, maprei, dyna
    character(len=19) :: chsol
!
    integer :: iforc0, iforc1
    character(len=24) :: cine
    integer :: iret
    cbid = dcmplx(0.d0, 0.d0)
!
!     -----------------------------------------------------------------
!
!====
! 1. CREATION DES STRUCTURES
!====
!
    call vtcreb(force0, 'V', 'R',&
                nume_ddlz = numedd,&
                nb_equa_outz = neq)
    call jeveuo(force0(1:19)//'.VALE', 'E', iforc0)
    call vtcreb(force1, 'V', 'R',&
                nume_ddlz = numedd,&
                nb_equa_outz = neq)
    call jeveuo(force1(1:19)//'.VALE', 'E', iforc1)
!
! 1.2. ==> NOM DES STRUCTURES DE TRAVAIL
!
    chsol = '&&DLTALI.SOLUTION'
    cine = ' '
    maprei = ' '
!
!====
! 2. L'INITIALISATION
!====
!
    inchac = 0
    lcrea = .true.
    call dltini(lcrea, nume, result, dep0, vit0,&
                acc0, fexte0, famor0, fliai0, neq,&
                numedd, inchac, baseno)
!
!
!====
! 4. --- CHARGEMENT A L'INSTANT INITIAL OU DE REPRISE ---
!====
!
    call dlfext(nveca, nchar, t0, neq, liad,&
                lifo, charge, infoch, fomult, modele,&
                mate, carele, numedd, zr(iforc0))
!
!====
! 5. --- CALCUL DU CHAMP D'ACCELERATION INITIAL ---
!====
!
    if (inchac .ne. 0) then
!
! 5.1. ==> --- RESOLUTION AVEC FORCE1 COMME SECOND MEMBRE ---
!
        call jeveuo(force1(1:19)//'.VALE', 'E', iforc1)
        call dcopy(neq, zr(iforc0), 1, zr(iforc1), 1)
        call dlfdyn(imat(1), imat(3), lamort, neq, dep0,&
                    vit0, zr(iforc1), tabwk)
!
        matrei = '&&MASSI'
        if (lprem) then
            lprem=.false.
            call ajlagr(rigid, masse, matrei)
!
! 5.2. ==> DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONEMENT
            call preres(solveu, 'V', icode, maprei, matrei,&
                        ibid, -9999)
        endif
!                                       ..          .
! 5.3. ==> RESOLUTION DU PROBLEME:  M.X  =  F - C.X - K.X
!                                       ..          .
!
        call resoud(matrei, maprei, solveu, cine, 0,&
                    force1, chsol, 'V', [0.d0], [cbid],&
                    criter, .true._1, 0, iret)
!
! 5.4. ==> SAUVEGARDE DU CHAMP SOLUTION CHSOL DANS VDEPL
!
        call copisd('CHAMP_GD', 'V', chsol(1:19), force1(1:19))
        call jeveuo(force1(1:19)//'.VALE', 'L', iforc1)
!
! 5.5. ==> DESTRUCTION DU CHAMP SOLUTION CHSOL
!
        call detrsd('CHAMP_GD', chsol)
!
! 5.6 ==> STOCKAGE DE LA SOLUTION, FORC1, DANS LA STRUCTURE DE RESULTAT
!           EN TANT QUE CHAMP D'ACCELERATION A L'INSTANT COURANT
        call dcopy(neq, zr(iforc1), 1, acc0, 1)
!
    endif
!
! CALCUL DE LA FORCE INITIALE SI PAS DE REPRISE A PARTIR D UN RESULTAT
!
!
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=dyna, nbret=ndy)
    if (ndy .eq. 0) then
        call wkvect('FEXT0M', 'V V R', neq, ifextm)
        call mrmult('ZERO', imat(1), dep0, fexte0, 1,&
                    .true._1)
        call mrmult('ZERO', imat(2), acc0, zr(ifextm), 1,&
                    .true._1)
        call wkvect('FEXT0C', 'V V R', neq, ifextc)
        if (lamort) then
            call mrmult('ZERO', imat(3), vit0, zr(ifextc), 1,&
                        .true._1)
        endif
        do ieq = 1, neq
            fexte0(ieq)=fexte0(ieq)+zr(ifextm-1+ieq) +zr(ifextc-1+ieq)
        end do
    endif
    call jedetr('FEXT0M')
    call jedetr('FEXT0C')
!
end subroutine
