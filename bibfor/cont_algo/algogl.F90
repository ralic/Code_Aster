subroutine algogl(ds_measure, sdcont_defi, sdcont_solv, solveu, matass,&
                  noma, ctccvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfacat.h"
#include "asterfort/cfaduc.h"
#include "asterfort/cfatmu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cffact.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cfmajc.h"
#include "asterfort/cfpeti.h"
#include "asterfort/cfreso.h"
#include "asterfort/cftabl.h"
#include "asterfort/elpiv1.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdsc3.h"
#include "asterfort/nmrvai.h"
#include "blas/daxpy.h"
!
! ======================================================================
! COPYRIGHT (C) 2005 IFP - MARTIN GUITTON         WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=8) :: noma
    character(len=24) :: sdcont_defi, sdcont_solv
    character(len=19) :: solveu, matass
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : CONTRAINTES TOUTES ACTIVES CAR GLISSIERE
! ALGO. POUR FROTTEMENT : SANS
! ALGO GLISSIERE
!
! ----------------------------------------------------------------------
!
!
! RESOLUTION DE : C.DU + AT.MU  = F
!                 A(U+DU)      <= E (POUR LES LIAISONS ACTIVES)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
!
!      A = MATRICE DE CONTACT
!
!      C = ( K  BT ) MATRICE DE RIGIDITE INCLUANT LES LAGRANGE
!          ( B  0  )
!
!      U = ( DEPL )
!          ( LAM  )
!
!      F = ( DL  ) DANS LA PHASE DE PREDICTION
!          ( DUD )
!
!      F = ( L - QT.SIG - BT.LAM  ) AU COURS D'UNE ITERATION DE NEWTON
!          (           0          )
!
! IO  ds_measure       : datastructure for measure and statistics management
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NOMA   : NOM DU MAILLAGE
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    integer :: ifm, niv
    aster_logical :: lechec
    integer :: ieq, iter
    integer :: llliai, llliac
    integer :: indic, indfac, ajliai, spliai, spavan
    integer :: neq, nbliac, nbliai, ndim, nesmax
    real(kind=8) :: rho, xjvmax
    character(len=1) :: typeaj
    character(len=19) :: macont
    integer :: ldscon, lmat
    character(len=19) :: ddeplc, ddepl0, ddelt
    integer :: itemax, isto, itemul
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddep0(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write(ifm,*) '<CONTACT><CALC> ALGO_CONTACT   : CONT. ACTIVES'
    endif
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddepl0 = sdcont_solv(1:14)//'.DEL0'
    ddeplc = sdcont_solv(1:14)//'.DELC'
    ddelt = sdcont_solv(1:14)//'.DDEL'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=ddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'E', vr=vddelt)
!
! --- PREPARATION DE LA MATRICE DE CONTACT
!
    macont = sdcont_solv(1:14)//'.MATC'
    call mtdsc3(macont)
    call jeecra(macont(1:19)//'.REFA', 'DOCU', cval='ASSE')
    call jeveuo(macont(1:19)//'.&INT', 'E', ldscon)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'L', lmat)
!
! --- INITIALISATION DES VARIABLES
!
    nbliai = cfdisd(sdcont_solv,'NBLIAI')
    neq = cfdisd(sdcont_solv,'NEQ' )
    ndim = cfdisd(sdcont_solv,'NDIM' )
    nesmax = cfdisd(sdcont_solv,'NESMAX')
    nbliac = cfdisd(sdcont_solv,'NBLIAC')
    itemul = 2
    itemax = itemul*nbliai
    typeaj = 'A'
    xjvmax = 0.d0
!
! --- GESTION DE LA FACTORISATION
!
    isto = cfdisi(sdcont_defi,'STOP_SINGULIER')
    ajliai = 0
    spliai = 0
    if (nbliac .gt. 0) then
        indic = 1
    else
        indic = 0
    endif
    indfac = 1
!
    iter = 1
!
    if (niv .ge. 2) then
        write(ifm,210) itemax
    endif
!
! ======================================================================
!                    REPRISE DE LA BOUCLE PRINCIPALE
! ======================================================================
!
 40 continue
!
! --- MISE A JOUR DE LA SOLUTION ITERATION DE CONTACT
!
    do ieq = 1, neq
        vddelt(ieq) = ddep0(ieq) - ddepc(ieq)
    end do
!
! --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
!
    if (nbliac .ne. 0) then
!
        spavan = spliai
!
! ----- CALCUL DE [-A.C-1.AT] COLONNE PAR COLONNE (A PARTIR DE INDFAC)
!
        call cfacat(indic, nbliac, ajliai, spliai,&
                    indfac, &
                    sdcont_defi, sdcont_solv, solveu, lmat, &
                    xjvmax)
!
! ----- DETECTION DES PIVOTS NULS
!
        call elpiv1(xjvmax, indic, nbliac, ajliai, spliai,&
                    spavan, noma, sdcont_defi, sdcont_solv)
!
! ----- ON A SUPPRIME UNE LIAISON
!
        if (indic .eq. -1) then
            goto 150
        endif
!
! ----- FACTORISATION LDLT DE [-A.C-1.AT]
!
        call cffact(ldscon, isto, nbliac, &
                    indfac, lechec)
!
! ----- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
!
        if (lechec) then
            ctccvg = 2
            goto 999
        endif
!
! ----- CALCUL DU SECOND MEMBRE : {JEU(DEPTOT) - A.DDEPL0} -> {MU}
!
        call cfaduc(sdcont_solv, nbliac)
!
! ----- RESOLUTION : [-A.C-1.AT].{MU} = {JEU(DEPTOT) - A.DDEPL0}
!
        call cfreso(sdcont_solv, ldscon, nbliac)
!
! ----- MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
!
        call cfmajc(sdcont_solv, neq, nbliac)
    endif
!
! --- VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES EST TROP PETIT
!
    call cfpeti(sdcont_solv, neq, nbliai, nbliac,&
                rho, llliai, llliac)
!
! --- ACTUALISATION DE DDEPLC = DDEPLC + RHO .DDELT
!
    call daxpy(neq, rho, vddelt, 1, ddepc, 1)
!
! --- MODIFICATIONS DES LIAISONS
!
    if (rho .lt. 1.d0) then
!
! ----- SI AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE
! ----- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE
!
        ASSERT(llliai.gt.0)
        call cftabl(indic, nbliac, ajliai, spliai,&
                    sdcont_solv, typeaj, llliac,&
                    llliai)
        call cfimp2(sdcont_defi, sdcont_solv, noma, llliai,&
                    'ACT')
    else
!
! ----- ON A CONVERGE
!
        goto 160
    endif
!
! --- ON PASSE A L'ITERATION DE CONTRAINTES ACTIVES SUIVANTE
!
150 continue
!
    iter = iter + 1
!
! --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
!
    if (iter .gt. itemax+1) then
        ctccvg = 1
        goto 999
    endif
!
    goto 40
!
! ======================================================================
!                            ON A CONVERGE
! ======================================================================
!
160 continue
!
! --- CALCUL DES FORCES DE CONTACT (AT.MU)
!
    call cfatmu(neq, nbliac, sdcont_solv)
!
! --- CODE RETOUR
!
    ctccvg = 0
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(sdcont_solv, 'NBLIAC', nbliac)
!
    if (niv .ge. 2) then
        write(ifm,220) iter
    endif
!
999 continue
!
! --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
!
    call nmrvai(ds_measure, 'Cont_Algo ', input_count = iter)
    call nmrvai(ds_measure, 'Cont_NCont', input_count = nbliac)
!
    call jedema()
!
210 format (' <CONTACT><CALC> DEBUT DES ITERATIONS (MAX: ',i6,')')
220 format (' <CONTACT><CALC> FIN DES ITERATIONS (NBR: ',i6,')')
!
end subroutine
