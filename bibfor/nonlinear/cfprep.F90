subroutine cfprep(noma, defico, resoco, matass, ddepla,&
                  depdel)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/cfdiag.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfjein.h'
    include 'asterfort/cfliin.h'
    include 'asterfort/cfprch.h'
    include 'asterfort/cfrsmu.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: matass
    character(len=19) :: ddepla, depdel
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! PREPARATION DES CALCULS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DDEPLA : INCREMENT DE DEPLACEMENT DEPUIS L'ITERATION
!              DE NEWTON PRECEDENTE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT DU PAS
!
!
!
!
    integer :: ifm, niv
    integer :: nbliai, ntpc, ndim
    integer :: iliai
    integer :: lmat
    character(len=19) :: liot, liac
    integer :: jliot, jliac
    character(len=19) :: mu, copo
    integer :: jmu, jcopo
    character(len=24) :: clreac
    integer :: jclrea
    logical :: lpenac, lctfd, lpenaf
    logical :: llagrc, llagrf, reapre, reageo
    real(kind=8) :: vdiagm
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... PREPARATION DU CALCUL'
    endif
!
! --- PARAMETRES
!
    nbliai = cfdisd(resoco,'NBLIAI' )
    ndim = cfdisd(resoco,'NDIM' )
    ntpc = cfdisi(defico,'NTPC' )
    lpenac = cfdisl(defico,'CONT_PENA' )
    lpenaf = cfdisl(defico,'FROT_PENA' )
    llagrc = cfdisl(defico,'CONT_LAGR' )
    llagrf = cfdisl(defico,'FROT_LAGR' )
    lctfd = cfdisl(defico,'FROT_DISCRET')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liot = resoco(1:14)//'.LIOT'
    liac = resoco(1:14)//'.LIAC'
    mu = resoco(1:14)//'.MU'
    clreac = resoco(1:14)//'.REAL'
    copo = resoco(1:14)//'.COPO'
    call jeveuo(liot, 'E', jliot)
    call jeveuo(liac, 'E', jliac)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(clreac, 'L', jclrea)
    call jeveuo(copo, 'E', jcopo)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE GLOBALE
!
    call jeveuo(matass//'.&INT', 'E', lmat)
!
! --- PARAMETRES DE REACTUALISATION
!
    reageo = zl(jclrea-1+1)
    reapre = zl(jclrea-1+3)
!
! --- INITIALISATIONS DES SD POUR LA DETECTION DES PIVOTS NULS
!
    if (llagrc) then
        if (reageo) then
            do 100 iliai = 1, ntpc
                zi(jliot+0*ntpc-1+iliai) = 0
                zi(jliot+1*ntpc-1+iliai) = 0
                zi(jliot+2*ntpc-1+iliai) = 0
                zi(jliot+3*ntpc-1+iliai) = 0
100          continue
            zi(jliot+4*ntpc ) = 0
            zi(jliot+4*ntpc+1) = 0
            zi(jliot+4*ntpc+2) = 0
            zi(jliot+4*ntpc+3) = 0
        endif
    else
        zi(jliot+4*nbliai ) = 0
        zi(jliot+4*nbliai+1) = 0
        zi(jliot+4*nbliai+2) = 0
        zi(jliot+4*nbliai+3) = 0
    endif
!
! --- INITIALISATIONS DES LAGRANGES
!
    if (llagrc .and. lctfd .and. reageo) then
        do 331 iliai = 1, ntpc
            zr(jmu+3*ntpc+iliai-1) = 0.d0
            zr(jmu+2*ntpc+iliai-1) = 0.d0
            zr(jmu+ ntpc+iliai-1) = 0.d0
            zr(jmu+ iliai-1) = 0.d0
331      continue
    endif
    if (lpenac .and. lpenaf .and. reapre) then
        do 332 iliai = 1, ntpc
            zr(jmu+2*ntpc+iliai-1) = 0.d0
            zr(jmu+ ntpc+iliai-1) = 0.d0
332      continue
    endif
!
    if (lpenac) then
        do 40 iliai = 1, ntpc
            zr(jmu+ iliai-1) = 0.d0
            if (lpenaf) then
                zr(jmu+3*ntpc+iliai-1) = 0.d0
            endif
40      continue
    endif
!
! --- RESTAURATION DU LAGRANGE DE CONTACT
! --- APRES UN APPARIEMENT
!
    if (reageo) then
        call cfrsmu(defico, resoco, reapre)
    endif
!
! --- PREPARATION DES CHAMPS
!
    call cfprch(defico, resoco, ddepla, depdel)
!
! --- SAUVEGARDE DE LA VALEUR MAXI SUR LA DIAGONALE DE LA
! --- MATR_ASSE DU SYSTEME
! --- POUR VALEUR DE LA PSEUDO-PENALISATION EN FROT. LAGR. 3D
!
    if (llagrf .and. (ndim.eq.3) .and. reapre) then
        call cfdiag(lmat, vdiagm)
        zr(jcopo) = vdiagm ** 0.25d0
    endif
!
! --- CALCUL DES JEUX INITIAUX
!
    call cfjein(noma, defico, resoco, depdel)
!
! --- LIAISONS INITIALES
!
    call cfliin(noma, defico, resoco)
!
    call jedema()
!
end subroutine
