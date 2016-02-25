subroutine fro2gd(ds_measure, defico, resoco, solveu, matass,&
                  noma, ctccvg)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfacat.h"
#include "asterfort/cfadh2.h"
#include "asterfort/cfaduf.h"
#include "asterfort/cfafmu.h"
#include "asterfort/cfatmu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cffact.h"
#include "asterfort/cfgli2.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cfmajf.h"
#include "asterfort/cfneg.h"
#include "asterfort/cfpeti.h"
#include "asterfort/cfreso.h"
#include "asterfort/cftabl.h"
#include "asterfort/elpiv2.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdsc3.h"
#include "asterfort/nmrvai.h"
#include "blas/daxpy.h"
    character(len=8) :: noma
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: defico, resoco
    character(len=19) :: solveu, matass
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
! ALGO. POUR FROTTEMENT : DUALISATION (LAGRANGIEN 2D)
!
! ----------------------------------------------------------------------
!
!
! RESOLUTION DE : C.DU + ACT.AC.MUC + ASGT.ASG.MUSG + AGT.AG.MUG = F
!                 AC. (U+DU)      <= E  (POUR LES LIAISONS ACTIVES)
!                 ASG.(U+DU)       = E' (POUR LES NOEUDS ADHERENTS)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
!
!     AC = MATRICE DE CONTACT
!
!    ASG = MATRICE DE FROTTEMENT POUR LES NOEUDS ADHERENTS
!
!    AG  = MATRICE DE FROTTEMENT POUR LES NOEUDS GLISSANTS
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
    integer :: ieq, iter
    integer :: llliai, llliac
    integer :: llf, llf1, llf2
    integer :: indic, indfac, ajliai, spliai
    aster_logical :: liasup, lechec
    integer :: nbpren
    integer :: neq, nbliac, nbliai, ndim, nesmax
    real(kind=8) :: rho, xjvmax
    character(len=1) :: typeaj
    character(len=2) :: typec0
    character(len=19) :: macont
    integer :: ldscon, lmat
    character(len=19) :: mu
    integer :: jmu
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
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write(ifm,*) '<CONTACT><CALC> ALGO_CONTACT   : DUALISATION'
        write(ifm,*) '<CONTACT><CALC> ALGO_FROTTEMENT: '//&
     &                'DUALISATION (2D)'
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    call jeveuo(mu, 'E', jmu)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=ddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'E', vr=vddelt)
!
! --- PREPARATION DE LA MATRICE DE CONTACT
!
    macont = resoco(1:14)//'.MATC'
    call mtdsc3(macont)
    call jeecra(macont(1:19)//'.REFA', 'DOCU', cval='ASSE')
    call jeveuo(macont(1:19)//'.&INT', 'E', ldscon)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'E', lmat)
!
! --- INITIALISATION DES VARIABLES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    ndim = cfdisd(resoco,'NDIM' )
    nesmax = cfdisd(resoco,'NESMAX')
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = cfdisd(resoco,'LLF' )
    llf1 = cfdisd(resoco,'LLF1' )
    llf2 = cfdisd(resoco,'LLF2' )
    itemul = cfdisi(defico,'ITER_CONT_MULT')
    itemax = itemul*nbliai
    typeaj = 'A'
    typec0 = 'C0'
    xjvmax = 0.d0
!
! --- GESTION DE LA FACTORISATION
!
    isto = cfdisi(defico,'STOP_SINGULIER')
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
        write(ifm,1001) itemax
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
    do 50 ieq = 1, neq
        vddelt(ieq) = ddep0(ieq) - ddepc(ieq)
 50 end do
!
! --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
!
    if (nbliac .ne. 0) then
!
! ----- CALCUL DE [-A.C-1.AT] COLONNE PAR COLONNE (A PARTIR DE INDFAC)
!
        call cfacat(ndim, indic, nbliac, ajliai, spliai,&
                    llf, llf1, llf2, indfac, nesmax,&
                    defico, resoco, solveu, lmat, nbliai,&
                    xjvmax)
!
! ----- DETECTION DES PIVOTS NULS
!
        call elpiv2(xjvmax, ndim, indic, nbliac, ajliai,&
                    spliai, llf, llf1, llf2, noma,&
                    defico, resoco)
!
! ----- ON A SUPPRIME UNE LIAISON
!
        if (indic .eq. -1) then
            goto 150
        endif
!
! ----- FACTORISATION LDLT DE [-A.C-1.AT]
!
        call cffact(ldscon, ndim, isto, nbliac, llf,&
                    llf1, llf2, indfac, lechec)
!
! ----- CALCUL DU SECOND MEMBRE : {JEU(DEPTOT) - A.DDEPL0} -> {MU}
!
        call cfaduf(resoco, ndim, nbliai, nbliac, llf,&
                    llf1, llf2)
!
! ----- RESOLUTION : [-A.C-1.AT].{MU} = {JEU(DEPTOT) - A.DDEPL0}
!
        call cfreso(resoco, ldscon, ndim, nbliac, llf,&
                    llf1, llf2)
!
! ----- MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
!
        call cfmajf(resoco, neq, ndim, nbliai, nbliac,&
                    llf, llf1, llf2)
!
! ----- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ?
!
        if (llf .lt. nbliac) then
            call cfgli2(noma, defico, resoco, neq, nbliai,&
                        nbliac, llf, ajliai, spliai, indic,&
                        liasup)
!
! ------- S'IL EXISTE AU MOINS UNE LIAISON ADHERENTE SUPPLEMENTAIRE,
! ------- ON NE PREND PAS EN COMPTE LES INCREMENTS DE DEPLACEMENTS
! ------- CALCULES ET ON RECOMMENCE LES CALCULS (NOTAMMENT POUR MU_SG)
!
            if (liasup) goto 40
        endif
!
! ----- LES LIAISONS CONSIDEREES ADHERENTES LE SONT-ELLES VRAIMENT ?
!
        if (llf .ne. 0) then
            call cfadh2(resoco, defico, noma, indic, nbliac,&
                        nbliai, ajliai, spliai, llf)
        endif
    endif
!
! --- VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES EST TROP PETIT
!
    call cfpeti(resoco, neq, nbliai, nbliac, llf,&
                llf1, llf2, rho, llliai, llliac)
!
! --- ACTUALISATION DE DELTA: DELTA = DELTA + RHO .DDELT
!
    call daxpy(neq, rho, vddelt, 1, ddepc,&
               1)
!
! --- AJOUT DE LA "PIRE" LIAISON SI NECESSAIRE
!
    if (rho .lt. 1.d0) then
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typeaj, llliac,&
                    llliai, typec0)
        call cfimp2(defico, resoco, noma, llliai, typec0,&
                    'ACT')
!
! ------ LA LIAISON EST SUPPOSEE GLISSANTE
!
        zr(jmu+3*nbliai+llliai-1) = 0.d0
!
    else
!
! ----- ON A CONVERGE
!
        goto 160
    endif
!
! --- ON PASSE A L'ITERATION DE CONTRAINTES ACTIVES SUIVANTES
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
! --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
! --- LA PRESSION EST NEGATIVE
!
    indic = 0
    call cfneg(resoco, defico, noma, ndim, indic,&
               nbliai, nbliac, ajliai, spliai, llf,&
               llf1, llf2, nbpren)
!
! --- CALCUL DES FORCES DE CONTACT (AT.MU)
!t
    call cfatmu(neq, nesmax, ndim, nbliac, 1,&
                llf, llf1, llf2, resoco)
!
! --- CALCUL DES FORCES DE FROTTEMENT (AF.MU)
!
    call cfafmu(resoco, neq, nbliai, nbliac, llf)
!
! --- CODE RETOUR
!
    ctccvg = 0
!
999 continue
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(resoco, 'NBLIAC', nbliac)
    call cfecrd(resoco, 'LLF', llf)
    call cfecrd(resoco, 'LLF1', llf1)
    call cfecrd(resoco, 'LLF2', llf2)
!
    if (niv .ge. 2) then
        write(ifm,1002) iter
    endif
!
! --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
!
    call nmrvai(ds_measure, 'Contact_Algo    ', input_count = iter)
    call nmrvai(ds_measure, 'Contact_NumbCont', input_count = nbliac)
    call nmrvai(ds_measure, 'Contact_NumbFric', input_count = llf)
!
    call jedema()
!
    1001 format (' <CONTACT><CALC> DEBUT DES ITERATIONS (MAX: ',i6,')')
    1002 format (' <CONTACT><CALC> FIN DES ITERATIONS (NBR: ',i6,')')
!
end subroutine
