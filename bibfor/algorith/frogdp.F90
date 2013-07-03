subroutine frogdp(sdstat, resoco, numedd, matass, resigr)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/cfcpem.h"
#include "asterfort/cfcpes.h"
#include "asterfort/cfcpma.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cffpfo.h"
#include "asterfort/cffpm1.h"
#include "asterfort/cffpm2.h"
#include "asterfort/cffrot.h"
#include "asterfort/cfmata.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nmrvai.h"
    real(kind=8) :: resigr
    character(len=24) :: sdstat
    character(len=24) :: resoco
    character(len=19) :: matass
    character(len=14) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : PENALISATION
! ALGO. POUR FROTTEMENT : PENALISATION
!
! ----------------------------------------------------------------------
!
!
! RESO. DE : C.DU + KC ACT.AC.DU + KG AGT.AG.DU = F - KG AGT.AG (E-U)
!            AC. (U+DU)      <= E  (= POUR LES LIAISONS ACTIVES)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
!
!     AC = MATRICE DE CONTACT
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
! IN  SDSTAT : SD STATISTIQUES
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NUMEDD : NUME_DDL
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  RESIGR : RESI_GLOB_RELA
! ON UTILISE UNIQUEMENT LE VECTEUR AFMU CAR LES DONNEES DE ATMU SONT
! NECESSAIRE POUR LE CALCUL DE LA MATRICE TANGENTE QUI SE FAIT
! A L'AIDE DU VECTEUR AFMU
!
!
!
!
    integer :: ifm, niv
    integer :: nmult
    integer :: ieq
    integer :: neq, nbliac, nbliai, ndim
    integer :: lmat
    integer :: lmaf1, iter
    integer :: nesmax
    character(len=14) :: numef1, numef2, nutemp
    character(len=19) :: maf1, maf2, matemp, mact
    character(len=14) :: numecf
    character(len=19) :: matrcf, fro1, fro2
    character(len=19) :: atmu, afmu
    integer :: jatmu, jafmu
    character(len=19) :: depl0
    integer :: jdepl0
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write(ifm,*) '<CONTACT><CALC> ALGO_CONTACT   : PENALISATION'
        write(ifm,*) '<CONTACT><CALC> ALGO_FROTTEMENT: PENALISATION'
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
    maf1 = '&&FROGDP.MAF1'
    numef1 = '&&FROGDP.NUF1'
    fro1 = resoco(1:14)//'.FRO1'
    numef2 = '&&FROGDP.NUF2'
    maf2 = '&&FROGDP.MAF2'
    fro2 = resoco(1:14)//'.FRO2'
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPL0: INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---        DU PAS DE TEMPS SANS CORRECTION DU CONTACT
!
    depl0 = resoco(1:14)//'.DEP0'
    call jeveuo(depl0 (1:19)//'.VALE', 'L', jdepl0)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'E', lmat)
!
! --- INITIALISATIONS DES VARIABLES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    ndim = cfdisd(resoco,'NDIM' )
    nesmax = cfdisd(resoco,'NESMAX')
    nbliac = cfdisd(resoco,'NBLIAC')
    iter = 1
!
! --- CREATION DU SECOND MEMBRE ATMU = -E_N.[Ac]T.{JEU}
!
    call cfcpes(resoco, jatmu)
!
    if (nbliac .eq. 0) then
        goto 999
    endif
!
! --- CALCUL DES COEFFICIENTS DE LAGRANGE MU POUR LE FROTTEMENT
!
    call cffpfo(resoco, nbliai, nbliac, ndim)
!
! --- CALCUL DE LA MATRICE DE CONTACT PENALISEE "ELEMENTAIRE" [E_N*AcT]
!
    call cfcpem(resoco, nbliai)
!
! --- CALCUL DE LA MATRICE DE CONTACT PENALISEE "GLOBALE" [E_N*AcT*Ac]
!
    mact = '&&FROGDP.MACT'
    call cfcpma(resoco, neq, nbliai, numedd, mact)
!
! --- CREATION DE LA MATRICE FRO1 = E_T*AaT
!
    call cffpm1(resoco, nbliai, ndim, nesmax)
!
! --- CREATION DE LA MATRICE MAF1 = E_T*AaT*Aa
! --- CETTE MATRICE NE SERT QU'AU CALCUL DU SECOND MEMBRE
!
    nmult = ndim - 1
    call cfmata(resoco, neq, nbliai, nmult, numedd,&
                fro1, numef1, maf1)
!
! --- RECUPERATION DU SECOND MEMBRE E_T*AaT*Aa * DELTA -> AFMU
!
    call mtdscr(maf1)
    call jeveuo(maf1//'.&INT', 'L', lmaf1)
    call mrmult('ZERO', lmaf1, zr(jdepl0), zr(jafmu), 1,&
                .true.)
!
! --- CREATION DE FRO2 = E_T*AT
!
    call cffpm2(resoco, resigr, nbliai, nbliac, ndim)
!
! --- CREATION DE LA SECONDE PARTIE DE LA MATRICE DE FROTTEMENT MAF2
!
    nmult = 1
    call cfmata(resoco, neq, nbliai, nmult, numedd,&
                fro2, numef2, maf2)
!
! --- CALCUL DE LA MATRICE TANGENTE MAFROT = MACT+MAF1+MAF2
!
    matemp = '&&FROGDP.MATP'
    nutemp = '&&FROGDP.NUTP'
    matrcf = resoco(1:14)//'.MATR'
    numecf = '&&FROGDP.NUFR'
    call cffrot(maf1, '-', maf2, matemp, nutemp)
    call cffrot(mact, '+', matemp, matrcf, numecf)
!
! --- CALCUL DES FORCES DE CONTACT (AT.MU) ET FROTTEMENT (AF.MU)
!
    do 350 ieq = 1, neq
        zr(jafmu-1+ieq) = zr(jafmu-1+ieq) + zr(jatmu-1+ieq)
        zr(jatmu-1+ieq) = 0.d0
350  end do
!
999  continue
!
! --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
!
    call cfecrd(resoco, 'NBLIAC', nbliac)
!
! --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
!
    call nmrvai(sdstat, 'CTCD_ALGO_ITER', 'E', iter)
    call nmrvai(sdstat, 'CONT_NBLIAC', 'E', nbliac)
!
    call jedema()
!
! ======================================================================
!
end subroutine
