subroutine mmmbca(noma, sddyna, iterat, defico, resoco,&
                  valinc, solalg, ctcsta, mmcvca)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfdist.h'
    include 'asterfort/cfmmco.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfnumm.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mcomce.h'
    include 'asterfort/mcopco.h'
    include 'asterfort/mmalgo.h'
    include 'asterfort/mmbouc.h'
    include 'asterfort/mmcycd.h'
    include 'asterfort/mmeven.h'
    include 'asterfort/mmextm.h'
    include 'asterfort/mmglis.h'
    include 'asterfort/mmimp4.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfm.h'
    include 'asterfort/mmmjev.h'
    include 'asterfort/mmnewj.h'
    include 'asterfort/mmnorm.h'
    include 'asterfort/mmstaf.h'
    include 'asterfort/mmvalp.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/vtgpld.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: valinc(*), solalg(*)
    character(len=19) :: sddyna
    logical :: mmcvca
    integer :: iterat, ctcsta
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! ALGO. DES CONTRAINTES ACTIVES POUR LE CONTACT METHODE CONTINUE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDDYNA : SD POUR DYNAMIQUE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT CTCSTA : NOMBRE DE POINTS AYANT CHANGE DE STATUT DE CONTACT
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
!
! ----------------------------------------------------------------------
!
    integer :: ibid
    integer :: ztabf
    integer :: ifm, niv
    integer :: jdecme, posmae, nummae, nummam, posnoe
    integer :: indcon, indcoi, indfrn, indfri
    integer :: izone, imae, iptc, iptm
    integer :: ndimg, nzoco
    integer :: nne, nptm, nbmae
    real(kind=8) :: ksipr1, ksipr2, ksipc1, ksipc2
    real(kind=8) :: geomp(3), geome(3)
    real(kind=8) :: vitpm(3), vitpe(3)
    real(kind=8) :: norm(3), tau1(3), tau2(3)
    real(kind=8) :: mlagc(9), mlagf1(9), mlagf2(9)
    real(kind=8) :: coorme(27)
    real(kind=8) :: lambdc(1), coefac, coefaf
    real(kind=8) :: noor, resen(3)
    real(kind=8) :: jeu, jeuvit, dist
    character(len=8) :: nommai, aliase
    character(len=19) :: cnsplu, cnsdel
    character(len=19) :: cnscon, cnsfr1, cnsfr2
    character(len=24) :: tabfin, jeusup, mdecol, apjeu
    integer :: jtabf, jjsup, jmdeco, japjeu
    character(len=19) :: oldgeo, newgeo
    character(len=19) :: chavit, chdepd
    character(len=19) :: depdel, depplu, vitplu
    logical :: lgliss, lvites, scotch
    logical :: lglini, lveri, lexig
    logical :: lfrotz, lpenaf, lfrot
    integer :: mmitgo, mmitfr, mmitca
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... ACTIVATION/DESACTIVATION'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lexig = cfdisl(defico,'EXIS_GLISSIERE')
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    jeusup = resoco(1:14)//'.JSUPCO'
    apjeu = resoco(1:14)//'.APJEU'
    call jeveuo(tabfin, 'E', jtabf)
    call jeveuo(jeusup, 'E', jjsup)
    call jeveuo(apjeu, 'E', japjeu)
    ztabf = cfmmvd('ZTABF')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- INDICATEUR DE DECOLLEMENT POUR LE THETA-SCHEMA
!
    mdecol = resoco(1:14)//'.MDECOL'
    call jeveuo(mdecol, 'E', jmdeco)
    scotch = zl(jmdeco+1-1)
!
! --- REACTUALISATION DE LA GEOMETRIE
!
    oldgeo = noma//'.COORDO'
    newgeo = resoco(1:14)//'.NEWG'
    call vtgpld('CUMU', oldgeo, 1.d0, depplu, 'V',&
                newgeo)
!
! --- POUR LA FORMULATION VITESSE, ON CREEE UN CHAMP DE VITESSE
!
    lvites = ndynlo(sddyna,'FORMUL_VITE')
    chavit = '&&MMMBCA.ACTUVIT'
    if (lvites) then
        call vtgpld('ZERO', oldgeo, 1.d0, vitplu, 'V',&
                    chavit)
    endif
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO')
    mmcvca = .true.
    lfrot = cfdisl(defico,'FROTTEMENT')
    posnoe = 0
    ctcsta = 0
    indfrn = 0
!
! --- TRANSFORMATION DEPPLU EN CHAM_NO_S ET REDUCTION SUR LES LAGRANGES
!
    cnsplu = '&&MMMBCA.CNSPLU'
    call cnocns(depplu, 'V', cnsplu)
    cnscon = '&&MMMBCA.CNSCON'
    call cnsred(cnsplu, 0, ibid, 1, 'LAGS_C',&
                'V', cnscon)
!
! --- TRANSFORMATION DEPDEL EN CHAM_NO_S ET REDUCTION SUR LES LAGRANGES
!
    cnsdel = '&&MMMBCA.CNSDEL'
    chdepd = '&&MMMBCA.CHDEPD'
    cnsfr1 = '&&MMMBCA.CNSFR1'
    cnsfr2 = '&&MMMBCA.CNSFR2'
    if (lfrot) then
        call cnocns(depdel, 'V', cnsdel)
        call cnsred(cnsdel, 0, ibid, 1, 'LAGS_F1',&
                    'V', cnsfr1)
        if (ndimg .eq. 3) then
            call cnsred(cnsdel, 0, ibid, 1, 'LAGS_F2',&
                        'V', cnsfr2)
        endif
        call vtgpld('ZERO', oldgeo, 1.d0, depdel, 'V',&
                    chdepd)
    endif
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do 10 izone = 1, nzoco
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        lgliss = mminfl(defico,'GLISSIERE_ZONE' ,izone )
        call cfmmco(defico, resoco, izone, 'COEF_AUGM_CONT', 'L',&
                    coefac)
        call cfmmco(defico, resoco, izone, 'COEF_AUGM_FROT', 'L',&
                    coefaf)
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
        lfrotz = mminfl(defico,'FROTTEMENT_ZONE',izone)
        lpenaf = mminfl(defico,'ALGO_FROT_PENA',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
            call cfnumm(defico, 1, posmae, nummae)
!
! ------- COORDONNNEES DES NOEUDS DE LA MAILLE ESCLAVE
!
            call mcomce(noma, newgeo, nummae, coorme, aliase,&
                        nne)
!
! ------- MULTIPLICATEURS DE CONTACT SUR LES NOEUDS ESCLAVES
!
            call mmextm(defico, cnscon, posmae, mlagc)
!
! ------- MULTIPLICATEURS DE FROTTEMENT SUR LES NOEUDS ESCLAVES
!
            if (lfrotz) then
                call mmextm(defico, cnsfr1, posmae, mlagf1)
                if (ndimg .eq. 3) then
                    call mmextm(defico, cnsfr2, posmae, mlagf2)
                endif
            endif
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- COORDONNEES ACTUALISEES DU POINT DE CONTACT
!
                ksipc1 = zr(jtabf+ztabf*(iptc-1)+3 )
                ksipc2 = zr(jtabf+ztabf*(iptc-1)+4 )
                call mmvalp(ndimg, aliase, nne, 3, ksipc1,&
                            ksipc2, coorme, geome)
!
! --------- COORDONNEES ACTUALISEES DE LA PROJECTION DU POINT DE CONTACT
!
                ksipr1 = zr(jtabf+ztabf*(iptc-1)+5 )
                ksipr2 = zr(jtabf+ztabf*(iptc-1)+6 )
                nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
                call mcopco(noma, newgeo, ndimg, nummam, ksipr1,&
                            ksipr2, geomp)
!
! --------- TANGENTES AU POINT DE CONTACT PROJETE SUR LA MAILLE MAITRE
!
                tau1(1) = zr(jtabf+ztabf*(iptc-1)+7 )
                tau1(2) = zr(jtabf+ztabf*(iptc-1)+8 )
                tau1(3) = zr(jtabf+ztabf*(iptc-1)+9 )
                tau2(1) = zr(jtabf+ztabf*(iptc-1)+10)
                tau2(2) = zr(jtabf+ztabf*(iptc-1)+11)
                tau2(3) = zr(jtabf+ztabf*(iptc-1)+12)
!
! --------- GLISSIERE ACTIVEE ?
!
                lglini = nint(zr(jtabf+ztabf*(iptc-1)+17)).eq.1
!
! --------- CALCUL DE LA NORMALE
!
                call mmnorm(ndimg, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call jenuno(jexnum(noma//'.NOMMAI', nummam), nommai)
                    call u2mesg('F', 'CONTACT3_23', 1, nommai, 0,&
                                0, 3, geomp)
                endif
!
! --------- STATUTS INITIAUX
!
                indcoi = nint(zr(jtabf+ztabf*(iptc-1)+22))
                indfri = nint(zr(jtabf+ztabf*(iptc-1)+23))
!
! --------- CALCUL DU JEU ACTUALISE AU POINT DE CONTACT
!
                call mmnewj(ndimg, geome, geomp, norm, jeu)
!
! --------- CALCUL DU JEU FICTIF AU POINT DE CONTACT
!
                call cfdist(defico, 'CONTINUE', izone, posnoe, posmae,&
                            geome, dist)
                zr(jjsup+iptc-1) = dist
!
! --------- JEU TOTAL
!
                zr(japjeu+iptc-1) = jeu+dist
                jeu = zr(japjeu+iptc-1)
!
! --------- NOEUDS EXCLUS -> ON SORT DIRECT
!
                if (zr(jtabf+ztabf*(iptc-1)+18) .eq. 1.d0) then
                    indcon = 0
                    indcoi = 0
                    goto 19
                endif
!
! --------- MULTIPLICATEUR DE LAGRANGE DE CONTACT DU POINT
!
                call mmvalp(ndimg, aliase, nne, 1, ksipc1,&
                            ksipc2, mlagc, lambdc)
!
! --------- FORMULATION EN VITESSE
!
                if (lvites) then
!
! ----------- COORDONNEES ACTUALISEES DU POINT DE CONTACT ET DU PROJETE
!
                    call mcopco(noma, chavit, ndimg, nummae, ksipc1,&
                                ksipc2, vitpe)
                    call mcopco(noma, chavit, ndimg, nummam, ksipr1,&
                                ksipr2, vitpm)
!
! ----------- CALCUL DU GAP DES VITESSES NORMALES
!
                    call mmmjev(ndimg, norm, vitpe, vitpm, jeuvit)
                endif
!
! --------- TRAITEMENT DES DIFFERENTS CAS DE CONTACT
!
                call mmalgo(indcoi, lvites, lglini, jeu, jeuvit,&
                            lambdc( 1), coefac, ctcsta, mmcvca, scotch,&
                            indcon)
!
! --------- STATUT DE FROTTEMENT
!
                if (lfrotz) then
                    call mmstaf(noma, ndimg, chdepd, coefaf, lpenaf,&
                                nummae, aliase, nne, nummam, ksipc1,&
                                ksipc2, ksipr1, ksipr2, mlagf1, mlagf2,&
                                tau1, tau2, norm, indcon, indfrn,&
                                resen)
                endif
!
19              continue
!
! --------- SAUVEGARDE STATUTS
!
                zr(jtabf+ztabf*(iptc-1)+22) = indcon
                if (lfrotz) then
                    zr(jtabf+ztabf*(iptc-1)+23) = indfrn
                    zr(jtabf+ztabf*(iptc-1)+25) = resen(1)
                    zr(jtabf+ztabf*(iptc-1)+26) = resen(2)
                    zr(jtabf+ztabf*(iptc-1)+27) = resen(3)
                endif
!
! --------- AFFICHAGE ETAT DU CONTACT
!
                if (niv .ge. 2) then
                    call mmimp4(ifm, noma, nummae, iptm, indcoi,&
                                indcon, indfri, indfrn, lfrot, lvites,&
                                lgliss, jeu, jeuvit, lambdc(1))
                endif
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
30          continue
20      continue
25      continue
10  end do
!
! --- GESTION DE LA GLISSIERE
!
    if (mmcvca .and. lexig) then
        call mmglis(defico, resoco)
    endif
!
! --- DETECTION DES CYCLAGES
!
    call mmcycd(noma, defico, resoco)
!
! --- GESTION DES EVENT POUR LA COLLISION
!
    call mmbouc(resoco, 'GEOM', 'READ', mmitgo)
    call mmbouc(resoco, 'FROT', 'READ', mmitfr)
    call mmbouc(resoco, 'CONT', 'READ', mmitca)
!
    if ((iterat.eq.0) .and. (mmitgo.eq.1) .and. (mmitfr.eq.1) .and. (mmitca.eq.1)) then
        call mmeven('INI', defico, resoco)
    else
        call mmeven('FIN', defico, resoco)
    endif
!
! --- SAUVEGARDE DECOLLEMENT POUR LE THETA-SCHEMA
!
    zl(jmdeco+1-1) = scotch
!
    call jedetr(newgeo)
    call jedetr(chavit)
    call jedetr(chdepd)
    call detrsd('CHAM_NO_S', cnsplu)
    call detrsd('CHAM_NO_S', cnsdel)
    call detrsd('CHAM_NO_S', cnscon)
    call detrsd('CHAM_NO_S', cnsfr1)
    call detrsd('CHAM_NO_S', cnsfr2)
!
    call jedema()
end subroutine
