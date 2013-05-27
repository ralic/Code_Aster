subroutine algocp(sdstat, resoco, numedd, matass)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/cfcpem.h'
    include 'asterfort/cfcpes.h'
    include 'asterfort/cfcpma.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfecrd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmrvai.h'
    character(len=24) :: sdstat, resoco
    character(len=19) :: matass
    character(len=14) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ALGO. POUR CONTACT    : PENALISATION
! ALGO. POUR FROTTEMENT : SANS
!
! ----------------------------------------------------------------------
!
!
! RESOLUTION DE : C.DU + K ATA.DU  = F- K ATA(E-U)
!                 A(U+DU)      <= E (= POUR LES LIAISONS ACTIVES)
!
! AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
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
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: afmu
    integer :: jafmu
    integer :: nbliai, neq, nbliac
    integer :: iter
    integer :: lmat
    character(len=19) :: matrcf
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
        write(ifm,*) '<CONTACT><CALC> ALGO_FROTTEMENT: SANS'
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    afmu = resoco(1:14)//'.AFMU'
    matrcf = resoco(1:14)//'.MATR'
    call jeveuo(afmu, 'E', jafmu)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'E', lmat)
!
! --- INITIALISATION DES VARIABLES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    nbliac = cfdisd(resoco,'NBLIAC')
    iter = 1
!
! --- CREATION DU SECOND MEMBRE AFMU = -E_N*AT*JEU
!
    call cfcpes(resoco, jafmu)
!
    if (nbliac .eq. 0) then
        goto 999
    endif
!
! --- CALCUL DE LA MATRICE DE CONTACT PENALISEE ELEMENTAIRE [E_N*AT]
!
    call cfcpem(resoco, nbliai)
!
! --- CALCUL DE LA MATRICE DE CONTACT PENALISEE GLOBALE [E_N*AT*A]
!
    call cfcpma(resoco, neq, nbliai, numedd, matrcf)
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
end subroutine
