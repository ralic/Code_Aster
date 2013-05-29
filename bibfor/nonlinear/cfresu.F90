subroutine cfresu(noma, numins, inst, sddisc, defico,&
                  resoco, depplu, depdel, ddepla, cnsinr,&
                  cnsper)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_20
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/r8miem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/caladu.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfimp3.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfresa.h'
    include 'asterfort/cfresb.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/iseven.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mmnorm.h'
    include 'asterfort/u2mess.h'
    integer :: numins
    real(kind=8) :: inst(*)
    character(len=8) :: noma
    character(len=19) :: sddisc
    character(len=19) :: cnsinr, cnsper
    character(len=24) :: defico, resoco
    character(len=19) :: depplu, depdel, ddepla
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - POST-TRAITEMENT)
!
! CREER LE CHAM_NO_S POUR ARCHIVAGE DU CONTACT PAR NMARCH
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMINS : NUMERO DU PAS DE CHARGE
! IN  INST   : INST(1) = INSTANT COURANT DE CALCUL
!              INST(2) = INCREMENT TEMPOREL
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  DEPPLU : DEPLACEMENT COURANT
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
! IN  DDEPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
! OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
! OUT CNSPER : CHAM_NO_S POUR L'ARCHIVAGE DES PERCUSSIONS
!
!
!
!
    integer :: zresu, zperc, ztacf
    integer :: ifm, niv
    integer :: numnoe, iliac, kk, icmp, iliai
    integer :: ndimg, nbliai, nbddl, neq
    integer :: btotal, nbliac, llf, llf1, llf2
    integer :: jdecal, lliac
    integer :: nesmax
    real(kind=8) :: glix, gliy, glit
    real(kind=8) :: testmu, testcf, coefpt
    real(kind=8) :: val1, val2, varc
    real(kind=8) :: r, rx, ry, rz
    real(kind=8) :: rn, rnx, rny, rnz, hn
    real(kind=8) :: rtax, rtay, rtaz, rtgx, rtgy, rtgz
    real(kind=8) :: tau1(3), tau2(3), norm(3), proj(3)
    real(kind=8) :: deltat, instap, r8bid
    character(len=2) :: typec0, typlia
    character(len=19) :: liac, atmu, afmu, mu, typl
    integer :: jliac, jatmu, jafmu, jmu, jtypl
    character(len=24) :: apddl, apcofr
    integer :: japddl, japcof
    character(len=24) :: tangco, tacfin
    integer :: jtango, jtacf
    character(len=24) :: appoin, numlia, approj
    integer :: japptr, jnumli, jappro
    character(len=24) :: jeuite
    integer :: jjeuit
    integer :: jcnsvr, jcnslr
    integer :: jdeplu, jdepde, jddepl
    logical :: lpenac, lctfd, lpenaf
    logical :: lag2d, lcolli, laffle
    real(kind=8) :: imp, impx, impy, impz
    real(kind=8) :: eps, valras
    integer :: jcnsvp, jcnslp
    parameter    (eps=1.d-6)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    instap = inst(1)
    deltat = inst(2)
    typec0 = 'C0'
!
! --- CONTACT AFFLEURANT EN MODE COLLISION
!
    laffle = .false.
    valras = 1.d-3
    call iseven(sddisc, 'COLLISION', lcolli)
!
! --- FONCTIONNALITES ACTIVEES
!
    lctfd = cfdisl(defico,'FROT_DISCRET')
    lpenac = cfdisl(defico,'CONT_PENA' )
    lpenaf = cfdisl(defico,'FROT_PENA' )
    lag2d = cfdisl(defico,'FROT_LAGR_2D')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    afmu = resoco(1:14)//'.AFMU'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    appoin = resoco(1:14)//'.APPOIN'
    numlia = resoco(1:14)//'.NUMLIA'
    atmu = resoco(1:14)//'.ATMU'
    typl = resoco(1:14)//'.TYPL'
    liac = resoco(1:14)//'.LIAC'
    mu = resoco(1:14)//'.MU'
    tangco = resoco(1:14)//'.TANGCO'
    tacfin = resoco(1:14)//'.TACFIN'
    jeuite = resoco(1:14)//'.JEUITE'
    approj = resoco(1:14)//'.APPROJ'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(atmu, 'L', jatmu)
    if (lctfd) then
        call jeveuo(apcofr, 'L', japcof)
        call jeveuo(afmu, 'L', jafmu)
    endif
    if (lpenac) then
        call jeveuo(afmu, 'L', jafmu)
    endif
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(tangco, 'L', jtango)
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(approj, 'L', jappro)
!
    zresu = cfmmvd('ZRESU')
    zperc = cfmmvd('ZPERC')
    ztacf = cfmmvd('ZTACF')
!
! --- CARACTERISTIQUES DU CONTACT
!
    nbliai = cfdisd(resoco,'NBLIAI')
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = cfdisd(resoco,'LLF' )
    llf1 = cfdisd(resoco,'LLF1' )
    llf2 = cfdisd(resoco,'LLF2' )
    ndimg = cfdisd(resoco,'NDIM' )
    neq = cfdisd(resoco,'NEQ' )
    nesmax = cfdisd(resoco,'NESMAX')
    btotal = nbliac+llf+llf1+llf2
!
! --- ACCES AUX CHAM_NO POUR LES DEPLACEMENTS
!
    call jeveuo(depplu(1:19)//'.VALE', 'L', jdeplu)
    call jeveuo(ddepla(1:19)//'.VALE', 'L', jddepl)
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
!
! --- ACCES AU CHAM_NO_S POUR LE CONTACT
!
    call jeveuo(cnsinr(1:19)//'.CNSV', 'E', jcnsvr)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'E', jcnslr)
!
! --- ACCES AU CHAM_NO_S POUR LES PERCUSSIONS
!
    call jeveuo(cnsper(1:19)//'.CNSV', 'E', jcnsvp)
    call jeveuo(cnsper(1:19)//'.CNSL', 'E', jcnslp)
!
! --- INITIALISATIONS DES CHAM_NO_S: NOEUDS ACTIFS
!
    do 10 iliai = 1, nbliai
        numnoe = zi(jnumli+4*(iliai-1)+3-1)
        do 11 icmp = 1, zresu
            zl(jcnslr-1+zresu*(numnoe-1)+icmp) = .true.
            zr(jcnsvr-1+zresu*(numnoe-1)+icmp) = 0.d0
11      continue
        do 12 icmp = 1, zperc
            zl(jcnslp-1+zperc*(numnoe-1)+icmp) = .true.
            zr(jcnsvp-1+zperc*(numnoe-1)+icmp) = 0.d0
12      continue
!
! ----- JEU
!
        zr(jcnsvr-1+zresu*(numnoe-1)+2 ) = zr(jjeuit+3*(iliai-1)+1-1)
10  end do
!
! ======================================================================
!
! --- CALCUL DES REACTIONS ET FORCES
!
! ======================================================================
!
    do 20 iliac = 1, btotal
!
! ----- NOEUD EN CONTACT
!
        varc = 2.0d0
!
! ----- INITIALISATIONS: CONTACT SANS FROTTEMENT
!
        rtax = 0.0d0
        rtay = 0.0d0
        rtaz = 0.0d0
        rtgx = 0.0d0
        rtgy = 0.0d0
        rtgz = 0.0d0
        hn = 0.0d0
!
! ----- REPERAGE DE LA LIAISON
!
        lliac = zi(jliac+iliac-1)
        jdecal = zi(japptr+lliac-1)
        nbddl = zi(japptr+lliac) - zi(japptr+lliac-1)
!
! ----- NUMERO DU NOEUD
!
        numnoe = zi(jnumli+4*(lliac-1)+3-1)
!
! ----- TANGENTES ET NORMALE
!
        tau1(1) = zr(jtango+6*(lliac-1)+1-1)
        tau1(2) = zr(jtango+6*(lliac-1)+2-1)
        tau1(3) = zr(jtango+6*(lliac-1)+3-1)
        tau2(1) = zr(jtango+6*(lliac-1)+4-1)
        tau2(2) = zr(jtango+6*(lliac-1)+5-1)
        tau2(3) = zr(jtango+6*(lliac-1)+6-1)
        call mmnorm(ndimg, tau1, tau2, norm, r8bid)
!
! --- CALCUL DES REACTIONS NORMALES DE CONTACT
!
        if (lpenac) then
            call cfresa(ndimg, zr(jafmu+zi(japddl+jdecal)-1), norm, rnx, rny,&
                        rnz, rn)
        else
            call cfresa(ndimg, zr(jatmu+zi(japddl+jdecal)-1), norm, rnx, rny,&
                        rnz, rn)
        endif
!
! ------ CONTACT AFFLEURANT ?
!
        if (rn .le. valras) then
            laffle = .true.
        endif
!
! --- CALCUL DES RESULTATS DU FROTTEMENT
!
        if (lctfd) then
!
! --- NOEUD SUPPOSE EN CONTACT GLISSANT
!
            varc = 2.0d0
!
! --- CALCUL DES GLISSEMENTS
!
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jdepde),&
                        val1)
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jddepl),&
                        val2)
            glix = val1 + val2
            gliy = 0.d0
!
            if (ndimg .eq. 3) then
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jdepde), val1)
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jddepl), val2)
                gliy = val1 + val2
            endif
!
            glit = sqrt(glix**2+gliy**2)
!
! --- CALCUL DES FORCES TANGENTIELLES
!
            if (lpenaf) then
!
! --- CAS DU FROTTEMENT PENALISE
!
!
! --- CALCUL DES FORCES TANGENTIELLES DE GLISSEMENT
!
                call cfresb(ndimg, lag2d, 'GL', zr(jafmu+zi(japddl+ jdecal)-1),&
                            zr(jtango+6*(lliac-1)), rtgx, rtgy, rtgz)
!
! --- DETERMINATION GLISSEMENT/ADHERENCE
!
                testmu = zr(jmu+3*nbliai+lliac-1)
                coefpt = zr(jtacf+ztacf*(lliac-1)+2)
                testcf = sqrt(coefpt)
                if (testcf .gt. r8miem()) then
                    if (abs((testmu-testcf)/testcf) .gt. r8prem()) then
                        varc = 1.0d0
                        rtax = rtgx
                        rtay = rtgy
                        rtaz = rtgz
                        rtgx = 0.0d0
                        rtgy = 0.0d0
                        rtgz = 0.0d0
                        hn = 0.0d0
                    else
                        varc = 2.0d0
                        rtax = 0.0d0
                        rtay = 0.0d0
                        rtaz = 0.0d0
                        hn = 0.0d0
                    endif
                endif
            else
!
! --- CAS DU FROTTEMENT DUALISE
!
                if (zk8(jtypl-1+iliac) .eq. typec0) then
                    do 50 kk = iliac+1, btotal
!
                        if (zi(jliac-1+kk) .eq. lliac) then
!
! --- NOEUD EN CONTACT ADHERENT
!
                            varc = 1.0d0
!
                            if (lag2d) then
                                typlia = '  '
                            else
                                typlia = zk8(jtypl-1+kk)(1:2)
                            endif
!
! --- CALCUL DES FORCES TANGENTIELLES D'ADHERENCE
!
!
                            call cfresb(ndimg, lag2d, typlia, zr(jatmu+ zi(japddl+jdecal)-1),&
                                        zr(jtango+6*(lliac- 1)), rtax, rtay, rtaz)
                            goto 100
                        endif
50                  continue
!
! --- NOEUD EN CONTACT GLISSANT
!
                    varc = 2.0d0
!
! --- CALCUL DES FORCES TANGENTIELLES DE GLISSEMENT
!
                    call cfresb(ndimg, lag2d, 'GL', zr(jafmu+zi(japddl+ jdecal)-1),&
                                zr(jtango+6*(lliac-1)), rtgx, rtgy, rtgz)
!
100                  continue
!
                else
!
! --- UNE LIAISON DE FROTTEMENT ADHERENT A DEJA ETE TRAITE
! --- DANS LA BOUCLE 50: ON NE RECOMMENCE PAS!
!
                    goto 150
!
                endif
            endif
        endif
!
! --- CALCUL DES FORCES RESULTANTES DE CONTACT/FROTTEMENT
!
        rx = rnx + rtax + rtgx
        ry = rny + rtay + rtgy
        rz = rnz + rtaz + rtgz
        r = sqrt(rx**2+ry**2+rz**2)
!
! --- ECRITURE DANS LE CHAM_NO
!
        zr(jcnsvr-1+zresu*(numnoe-1)+1 ) = varc
        zr(jcnsvr-1+zresu*(numnoe-1)+3 ) = rn
        zr(jcnsvr-1+zresu*(numnoe-1)+4 ) = rnx
        zr(jcnsvr-1+zresu*(numnoe-1)+5 ) = rny
        zr(jcnsvr-1+zresu*(numnoe-1)+6 ) = rnz
        zr(jcnsvr-1+zresu*(numnoe-1)+16) = rx
        zr(jcnsvr-1+zresu*(numnoe-1)+17) = ry
        zr(jcnsvr-1+zresu*(numnoe-1)+18) = rz
        zr(jcnsvr-1+zresu*(numnoe-1)+19) = r
        zr(jcnsvr-1+zresu*(numnoe-1)+20) = hn
!
! ------ COORDONNNES DE LA PROJECTION
!
        proj(1) = zr(jappro+3*(lliac-1)+1-1)
        proj(2) = zr(jappro+3*(lliac-1)+2-1)
        proj(3) = zr(jappro+3*(lliac-1)+3-1)
        zr(jcnsvr-1+zresu*(numnoe-1)+28) = proj(1)
        zr(jcnsvr-1+zresu*(numnoe-1)+29) = proj(2)
        zr(jcnsvr-1+zresu*(numnoe-1)+30) = proj(3)
!
! ------ CALCUL DES PERCUSSIONS
!
        if (r .le. eps) then
!
            imp = 0.d0
            impx = 0.d0
            impy = 0.d0
            impz = 0.d0
            zr(jcnsvp-1+zperc*(numnoe-1)+1) = 0.d0
            zr(jcnsvp-1+zperc*(numnoe-1)+2) = 0.d0
            zr(jcnsvp-1+zperc*(numnoe-1)+3) = 0.d0
            zr(jcnsvp-1+zperc*(numnoe-1)+4) = 0.d0
!
        else
!
            imp = zr(jcnsvp-1+zperc*(numnoe-1)+1) + r*deltat
            zr(jcnsvp-1+zperc*(numnoe-1)+1) = imp
!
            impx = zr(jcnsvp-1+zperc*(numnoe-1)+2) + rx*deltat
            zr(jcnsvp-1+zperc*(numnoe-1)+2) = impx
!
            impy = zr(jcnsvp-1+zperc*(numnoe-1)+3) + ry*deltat
            zr(jcnsvp-1+zperc*(numnoe-1)+3) = impy
!
            impz = zr(jcnsvp-1+zperc*(numnoe-1)+4) + rz*deltat
            zr(jcnsvp-1+zperc*(numnoe-1)+4) = impz
!
        endif
!
        zr(jcnsvr-1+zresu*(numnoe-1)+21)= imp
        zr(jcnsvr-1+zresu*(numnoe-1)+22)= impx
        zr(jcnsvr-1+zresu*(numnoe-1)+23)= impy
        zr(jcnsvr-1+zresu*(numnoe-1)+24)= impz
!
! ------ DONNEES DU FROTTEMENT
!
        if (lctfd) then
            zr(jcnsvr-1+zresu*(numnoe-1)+7 ) = glix
            zr(jcnsvr-1+zresu*(numnoe-1)+8 ) = gliy
            zr(jcnsvr-1+zresu*(numnoe-1)+9 ) = glit
            zr(jcnsvr-1+zresu*(numnoe-1)+10) = rtax
            zr(jcnsvr-1+zresu*(numnoe-1)+11) = rtay
            zr(jcnsvr-1+zresu*(numnoe-1)+12) = rtaz
            zr(jcnsvr-1+zresu*(numnoe-1)+13) = rtgx
            zr(jcnsvr-1+zresu*(numnoe-1)+14) = rtgy
            zr(jcnsvr-1+zresu*(numnoe-1)+15) = rtgz
        endif
!
! ------ COORDONNEES DU POINT DE CONTACT
!
        zr(jcnsvr-1+zresu*(numnoe-1)+25)= 0.d0
        zr(jcnsvr-1+zresu*(numnoe-1)+26)= 0.d0
        zr(jcnsvr-1+zresu*(numnoe-1)+27)= 0.d0
!
150      continue
20  end do
!
! --- ECRITURE DES RELATIONS DE CONTACT A LA FIN DU PAS DE TEMPS
!
    if (niv .ge. 2) then
        call cfimp3(defico, resoco, noma, ifm, numins,&
                    instap, nbliai, nbliac, jcnsvr)
    endif
!
! --- ALARME SI CONTACT AFFLEURANT EN MODE DETECTION COLLISION
!
    if (laffle .and. lcolli) then
        call u2mess('A', 'CONTACT3_98')
    endif
!
    call jedema()
! ======================================================================
end subroutine
