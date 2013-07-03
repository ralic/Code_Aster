subroutine merimo(base, modele, carele, mate, comref,&
                  compor, carcri, iterat, fonact, sddyna,&
                  valinc, solalg, merigi, vefint, optioz,&
                  tabret, codere)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/merimp.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmiret.h"
#include "asterfort/reajre.h"
#include "asterfort/redetr.h"
    integer :: iterat
    logical :: tabret(0:10)
    integer :: fonact(*)
    character(len=*) :: mate, optioz
    character(len=1) :: base
    character(len=19) :: sddyna
    character(len=24) :: modele, carele, compor, comref
    character(len=24) :: carcri, codere
    character(len=19) :: merigi, vefint
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE
! CALCUL DES VECTEURS ELEMENTAIRES DES FORCES INTERNES
!
! ----------------------------------------------------------------------
!
!
! IN  BASE   : BASE 'V' OU 'G' OU SONT CREES LES OBJETS EN SORTIE
! IN  MODELE : NOM DU MODELE
! IN  CARELE : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE   : CHAMP DE MATERIAU CODE
! IN  COMPOR : TYPE DE RELATION DE COMPORTEMENT
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  OPTION : OPTION DEMANDEE
! IN  ITERAT : NUMERO D'ITERATION INTERNE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  COMREF : VALEURS DE REF DES VAR DE COMMANDE (TREF, ...)
! OUT MERIGI : MATRICES ELEMENTAIRES DE RIGIDITE
! OUT VEFINT : VECTEURS ELEMENTAIRES DES FORCES INTERIEURES
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
! OUT TABRET : TABLEAU RESUMANT LES CODES RETOURS DU TE
!                    TABRET(0) = .TRUE. UN CODE RETOUR NON NUL EXISTE
!                    TABRET(I) = .TRUE. CODE RETOUR I RENCONTRE
!                                SINON .FALSE.
!                    I VALANT DE 1 A 10
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=9, nbin=56)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    logical :: lmacr
    logical :: matrix, vector, codint, conext
    integer :: ires, iarefe, iret
    character(len=24) :: caco3d
    character(len=24) :: ligrmo
    character(len=19) :: sigext, sigplu, varplu, strplu
    character(len=16) :: option
    logical :: debug
    integer :: ifmdbg, nivdbg
!
    data caco3d/'&&MERIMO.CARA_ROTA_FICTI'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    option = optioz
    ligrmo = modele(1:8)//'.MODELE'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    do 10 iret = 0, 10
        tabret(iret) = .false.
10  end do
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'SIGEXT', sigext)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'STRPLU', strplu)
!
! --- PREPARATION DES CHAMPS D'ENTREE POUR RIGIDITE TANGENTE (MERIMO)
!
    call merimp(modele, carele, mate, comref, compor,&
                carcri, iterat, sddyna, valinc, solalg,&
                caco3d, nbin, lpain, lchin)
!
! --- TYPE DE SORTIES:
! --- MATRIX: MATRICE TANGENTE
! --- VECTOR: FORCES INTERIEURES
! --- CODINT: CODE RETOUR ERREUR INTEG. LDC
!
    if (option(1:9) .eq. 'FULL_MECA') then
        matrix = .true.
        vector = .true.
        codint = .true.
        conext = .false.
    else if (option(1:10).eq.'RIGI_MECA ') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .false.
    else if (option(1:16).eq.'RIGI_MECA_IMPLEX') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .true.
    else if (option(1:10).eq.'RIGI_MECA_') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .false.
    else if (option(1:9).eq.'RAPH_MECA') then
        matrix = .false.
        vector = .true.
        codint = .true.
        conext = .false.
    else
        call assert(.false.)
    endif
!
! --- AFFICHAGE
!
    if (nivdbg .ge. 2) then
        write (ifmdbg,*) '<CALCUL> ... OPTION: ',option
        if (matrix) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES MATR_ELEM '//&
            ' DE RIGIDITE '
        endif
        if (vector) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES VECT_ELEM '//&
            ' DES FORCES INTERNES '
        endif
        if (conext) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES CONTRAINTES '//&
            ' EXTRAPOLEES POUR IMPLEX '
        endif
    endif
!
! --- PREPARATION DES MATR_ELEM ET VECT_ELEM
!
    if (matrix) then
        call jeexin(merigi//'.RELR', iret)
        if (iret .eq. 0) then
            call jeexin(merigi//'.RERR', ires)
            if (ires .eq. 0) then
                call memare(base, merigi, modele(1:8), mate, carele,&
                            'RIGI_MECA')
            endif
            if (lmacr) then
                call jeveuo(merigi//'.RERR', 'E', iarefe)
                zk24(iarefe-1+3) = 'OUI_SOUS_STRUC'
            endif
        endif
        call jedetr(merigi//'.RELR')
        call reajre(merigi, ' ', base)
    endif
!
    if (vector) then
        call jeexin(vefint//'.RELR', iret)
        if (iret .eq. 0) then
            call memare(base, vefint, modele(1:8), mate, carele,&
                        'CHAR_MECA')
        endif
        call jedetr(vefint//'.RELR')
        call reajre(vefint, ' ', base)
    endif
!
! --- CHAMPS DE SORTIE
!
    lpaout(4) = 'PCONTPR'
    lchout(4) = sigplu(1:19)
    lpaout(5) = 'PVARIPR'
    lchout(5) = varplu(1:19)
    lpaout(7) = 'PCACO3D'
    lchout(7) = caco3d(1:19)
    lpaout(9) = 'PSTRXPR'
    lchout(9) = strplu(1:19)
    if (matrix) then
        lpaout(1) = 'PMATUUR'
        lchout(1) = merigi(1:15)//'.M01'
        lpaout(2) = 'PMATUNS'
        lchout(2) = merigi(1:15)//'.M02'
    endif
    if (vector) then
        lpaout(3) = 'PVECTUR'
        lchout(3) = vefint(1:15)//'.R01'
    endif
    if (codint) then
        lpaout(6) = 'PCODRET'
        lchout(6) = codere(1:19)
    endif
    if (conext) then
        lpaout(8) = 'PCONTXR'
        lchout(8) = sigext(1:19)
    endif
!
! --- APPEL A CALCUL
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'NON')
!
! --- SAUVEGARDE MATR_ELEM/VECT_ELEM
!
    if (matrix) then
        call reajre(merigi, lchout(1), base)
        call reajre(merigi, lchout(2), base)
!       -- DESTRUCTION DES RESUELEM NULS :
        call redetr(merigi)
    endif
!
    if (vector) then
        call reajre(vefint, lchout(3), base)
    endif
!
! --- SAUVEGARDE CODE RETOUR ERREUR
!
    if (codint) then
        call nmiret(lchout(6), tabret)
    endif
!
    call jedema()
end subroutine
