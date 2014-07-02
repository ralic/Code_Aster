subroutine fropgd(sdstat, defico, resoco, solveu, numedd,&
                  matass, noma, resigr, depdel, ctccvg,&
                  ctcfix)
!
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfacat.h"
#include "asterfort/cfaduf.h"
#include "asterfort/cfatmu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cffact.h"
#include "asterfort/cffllf.h"
#include "asterfort/cfflm1.h"
#include "asterfort/cfflm2.h"
#include "asterfort/cffrot.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cfmajf.h"
#include "asterfort/cfmata.h"
#include "asterfort/cfneg.h"
#include "asterfort/cfpeti.h"
#include "asterfort/cfpppg.h"
#include "asterfort/cfreso.h"
#include "asterfort/cftabl.h"
#include "asterfort/elpiv1.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdsc3.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nmrvai.h"
#include "blas/daxpy.h"
    real(kind=8) :: resigr
    character(len=8) :: noma
    character(len=24) :: sdstat
    character(len=24) :: defico, resoco
    character(len=19) :: solveu, matass, depdel
    character(len=14) :: numedd
    integer :: ctccvg
    aster_logical :: ctcfix
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
! ALGO. POUR FROTTEMENT : PENALISATION
!
! ----------------------------------------------------------------------
!
!
! RESOLUTION DE : C.DU + ACT.MUC     = F - KG AGT.AG (E-U)
!                      AC. (U+DU)   <= E  (= POUR LES LIAISONS ACTIVES)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
!
!     AC = MATRICE DE CONTACT
!
!     AG = MATRICE DE FROTTEMENT POUR LES NOEUDS GLISSANTS
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
! IN  NUMEDD : NUME_DDL
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NOMA   : NOM DU MAILLAGE
! OUT CTCFIX : .TRUE.  SI ATTENTE POINT FIXE CONTACT
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
! IN  RESIGR : RESI_GLOB_RELA
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
    aster_logical :: lechec
    integer :: nbpren
    character(len=14) :: numef1, numef2
    character(len=19) :: maf1, maf2
    character(len=14) :: numecf
    integer :: neq, nbliac, nbliai, ndim, nesmax
    real(kind=8) :: rho, xjvmax
    character(len=1) :: typeaj
    character(len=2) :: typec0
    character(len=19) :: macont
    integer :: ldscon, lmat, lmaf1
    character(len=19) :: matrcf, fro1, fro2
    integer :: nmult
    character(len=19) :: atmu, afmu
    integer :: jatmu, jafmu
    character(len=19) :: ddeplc, ddepl0, ddelt, deplc
    integer :: itemax, isto, itemul, spavan
    real(kind=8) :: glitol, glimin, glimax
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddep0(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
    real(kind=8), pointer :: depc(:) => null()
    real(kind=8), pointer :: depde(:) => null()
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
     &                'PENALISATION'
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    atmu = resoco(1:14)//'.ATMU'
    afmu = resoco(1:14)//'.AFMU'
    call jeveuo(atmu, 'E', jatmu)
    call jeveuo(afmu, 'E', jafmu)
!
! --- MATRICES DE FROTTEMENT
!
    maf1 = '&&FROPGD.MAF1'
    numef1 = '&&FROPGD.NUF1'
    fro1 = resoco(1:14)//'.FRO1'
    numef2 = '&&FROPGD.NUF2'
    maf2 = '&&FROPGD.MAF2'
    fro2 = resoco(1:14)//'.FRO2'
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=ddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'E', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'E', vr=vddelt)
    call jeveuo(deplc (1:19)//'.VALE', 'E', vr=depc)
    call jeveuo(depdel(1:19)//'.VALE', 'L', vr=depde)
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
    glitol = 1.d-08
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
! --- CALCUL DE DEPLC = DEPDEL + DDELT
!
    do 240 ieq = 1, neq
        depc(ieq) = depde(ieq) + ddepc(ieq)
240 end do
!
! --- CALCUL DES FORCES DE CONTACT (AT.MU)
!
    call cfatmu(neq, nesmax, ndim, nbliac, 1,&
                llf, llf1, llf2, resoco)
!
! --- PAS DE CONTACT -> ON SORT
!
    if (nbliac .eq. 0) then
        ctccvg = 0
        goto 999
    endif
!
! --- DETERMINATION DU PLUS GRAND ET DU PLUS PETIT GLISSEMENT TANGENT
!
    call cfpppg(resoco, ndim, neq, nesmax, nbliac,&
                glimin, glimax)
!
! --- CALCUL DES COEFFICIENTS DE LAGRANGE MU POUR LE FROTTEMENT
!
    call cffllf(resoco, ndim, neq, nesmax, nbliac,&
                nbliai, glitol, glimin, glimax)
!
! --- CREATION DE LA MATRICE FRO1 = E_T*AaT (TERME POSITIF)
!
    call cfflm1(resoco, ndim, nesmax, nbliai, nbliac)
!
! --- CREATION DE LA MATRICE MAF1 = E_T*AaT*Aa
! --- CETTE MATRICE SERT AUSSI AU CALCUL DU SECOND MEMBRE
!
    nmult = ndim - 1
    call cfmata(resoco, neq, nbliai, nmult, numedd,&
                fro1, numef1, maf1)
!
! --- RECUPERATION DU SECOND MEMBRE MAF1 * DEPLC -> AFMU
!
    call mtdscr(maf1)
    call jeveuo(maf1//'.&INT', 'L', lmaf1)
    call mrmult('ZERO', lmaf1, depc, zr(jafmu), 1,&
                .true._1)
!
! --- CREATION DE LA MATRICE FRO2 = E_T*AT (TERME NEGATIF)
!
    call cfflm2(resoco, resigr, ndim, neq, nesmax,&
                nbliac, nbliai, glitol, glimin, glimax)
!
! --- CREATION DE LA MATRICE DE FROTTEMENT - SECONDE PARTIE (MAF2)
!
    nmult = 1
    call cfmata(resoco, neq, nbliai, nmult, numedd,&
                fro2, numef2, maf2)
!
! --- CALCUL DE LA MATRICE TANGENTE RESULTANTE
!
    matrcf = resoco(1:14)//'.MATR'
    numecf = '&&FROPGD.NUFR'
    call cffrot(maf1, '-', maf2, matrcf, numecf)
!
! --- ATTENTE POINT FIXE
!
    if ((nbpren.ne.0) .and. (indic.ne.0)) then
        ctcfix = .true.
    endif
!
999 continue
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(resoco, 'NBLIAC', nbliac)
!
    if (niv .ge. 2) then
        write(ifm,1002) iter
    endif
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
