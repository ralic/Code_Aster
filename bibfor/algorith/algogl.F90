subroutine algogl(sdstat, defico, resoco, solveu, matass,&
                  noma, ctccvg)
!
! ======================================================================
! COPYRIGHT (C) 2005 IFP - MARTIN GUITTON         WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    implicit     none
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
    character(len=24) :: sdstat
    character(len=8) :: noma
    character(len=24) :: defico, resoco
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
! IN  SDSTAT : SD STATISTIQUES
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
    logical :: lechec
    integer :: ibid, ieq, iter
    integer :: llliai, llliac
    integer :: llf, llf1, llf2
    integer :: indic, indfac, ajliai, spliai, spavan
    integer :: neq, nbliac, nbliai, ndim, nesmax
    real(kind=8) :: rho, xjvmax
    character(len=1) :: typeaj
    character(len=2) :: typec0
    character(len=19) :: macont
    integer :: ldscon, lmat
    character(len=19) :: ddeplc, ddepl0, ddelt
    integer :: jddepc, jddep0, jddelt
    integer :: itemax, isto, itemul
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write(ifm,*) '<CONTACT><CALC> ALGO_CONTACT   : CONT. ACTIVES'
        write(ifm,*) '<CONTACT><CALC> ALGO_FROTTEMENT: SANS'
    endif
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', jddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', jddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'E', jddelt)
!
! --- PREPARATION DE LA MATRICE DE CONTACT
!
    macont = resoco(1:14)//'.MATC'
    call mtdsc3(macont)
    call jeecra(macont(1:19)//'.REFA', 'DOCU', ibid, 'ASSE')
    call jeveuo(macont(1:19)//'.&INT', 'E', ldscon)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'L', lmat)
!
! --- INITIALISATION DES VARIABLES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    ndim = cfdisd(resoco,'NDIM' )
    nesmax = cfdisd(resoco,'NESMAX')
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = 0
    llf1 = 0
    llf2 = 0
    itemul = 2
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
40  continue
!
! --- MISE A JOUR DE LA SOLUTION ITERATION DE CONTACT
!
    do 50 ieq = 1, neq
        zr(jddelt+ieq-1) = zr(jddep0+ieq-1) - zr(jddepc-1+ieq)
50  end do
!
! --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
!
    if (nbliac .ne. 0) then
!
        spavan = spliai
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
        call elpiv1(xjvmax, indic, nbliac, ajliai, spliai,&
                    spavan, noma, defico, resoco)
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
! ----- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
!
        if (lechec) then
            ctccvg = 2
            goto 999
        endif
!
! ----- CALCUL DU SECOND MEMBRE : {JEU(DEPTOT) - A.DDEPL0} -> {MU}
!
        call cfaduc(resoco, nbliac)
!
! ----- RESOLUTION : [-A.C-1.AT].{MU} = {JEU(DEPTOT) - A.DDEPL0}
!
        call cfreso(resoco, ldscon, ndim, nbliac, llf,&
                    llf1, llf2)
!
! ----- MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
!
        call cfmajc(resoco, neq, nbliac)
    endif
!
! --- VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES EST TROP PETIT
!
    call cfpeti(resoco, neq, nbliai, nbliac, llf,&
                llf1, llf2, rho, llliai, llliac)
!
! --- ACTUALISATION DE DDEPLC = DDEPLC + RHO .DDELT
!
    call daxpy(neq, rho, zr(jddelt), 1, zr(jddepc),&
               1)
!
! --- MODIFICATIONS DES LIAISONS
!
    if (rho .lt. 1.d0) then
!
! ----- SI AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE
! ----- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE
!
        ASSERT(llliai.gt.0)
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typeaj, llliac,&
                    llliai, typec0)
        call cfimp2(defico, resoco, noma, llliai, typec0,&
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
150  continue
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
160  continue
!
! --- CALCUL DES FORCES DE CONTACT (AT.MU)
!
    call cfatmu(neq, nesmax, ndim, nbliac, 0,&
                llf, llf1, llf2, resoco)
!
! --- CODE RETOUR
!
    ctccvg = 0
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(resoco, 'NBLIAC', nbliac)
!
    if (niv .ge. 2) then
        write(ifm,1002) iter
    endif
!
999  continue
!
! --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
!
    call nmrvai(sdstat, 'CTCD_ALGO_ITER', 'E', iter)
    call nmrvai(sdstat, 'CONT_NBLIAC', 'E', nbliac)
!
    call jedema()
!
    1001 format (' <CONTACT><CALC> DEBUT DES ITERATIONS (MAX: ',i6,')')
    1002 format (' <CONTACT><CALC> FIN DES ITERATIONS (NBR: ',i6,')')
!
end subroutine
