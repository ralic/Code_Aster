subroutine te0466(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/dimthm.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option, nomte
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! =====================================================================
! person_in_charge: sylvie.granet at edf.fr
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
!
! TOLE CRP_20
!
!     BUT: CALCUL DES VECTEURS ELEMENTA EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN FLUX NORMAUX HYDRAULIQUES
!          ET THERMIQUES SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES
!          3D_THHM, 3D_THM,3D_THH,3D_HHM,3D_HM, 3D_HH
!          ACTUELLEMENT TRAITES : FACE8 ET FACE6
!          OPTIONS : 'CHAR_MECA_FLUX_R' ET 'CHAR_MECA_FLUX_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j, l, ifluxf
    integer :: ndim, nno, ipg, npi, ires, iflux, itemps, jgano
    integer :: idec, jdec, kdec, ldec, ldec2, ino, jno, ibid
    integer :: iopt, iret, iforc
    integer :: nno2, nnos2, ipoid2, idfdx2, ivf2
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, valpar(4)
    real(kind=8) :: deltat, flu1, flu2, fluth, x, y, z, fx, fy, fz
    integer :: napre1, napre2, natemp, ndim2
    character(len=8) :: nompar(4), elrefe
    character(len=24) :: elref2
    integer :: ndlno, ndlnm, ipres, ipresf
    real(kind=8) :: pres, presf
!
    integer :: nnos, npi2
    integer :: nflux
!------------------------------------------------------------------
!  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH ,THM,
!                           THH2M, HH2M,  THH2, HH, HH2
!------------------------------------------------------------------
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    ndim2 = 3
    call elref1(elrefe)
    if (elrefe .eq. 'TR6') then
        elref2 = 'TR3'
    else if (elrefe.eq.'QU8') then
        elref2 = 'QU4'
    else if (elrefe.eq.'QU9') then
        elref2 = 'QU4'
!            WRITE(6,*)'ELREFE2',ELREF2
    else
        call u2mesk('F', 'DVP_4', 1, elrefe)
    endif
! FONCTIONS DE FORMES QUADRATIQUES
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npi, ipoids, ivf, idfdx, jgano)
! FONCTIONS DE FORMES LINEAIRES
    call elref4(elref2, 'RIGI', ndim, nno2, nnos2,&
                npi2, ipoid2, ivf2, idfdx2, ibid)
!
! NB DE DDL A CHAQUE NOEUD
    call dimthm(nomte, ndlno, ndlnm, ndim2)
!      WRITE(6,*)'NDLNO',NDLNO
!      WRITE(6,*)'NDLNM',NDLNM
!      WRITE(6,*)'NDIM2',NDIM2
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ires)
!
    if (option .eq. 'CHAR_MECA_FLUX_R') then
!         WRITE(6,*)'OPTION',OPTION
        iopt = 1
        call jevech('PFLUXR', 'L', iflux)
        call jevech('PTEMPSR', 'L', itemps)
        deltat = zr(itemps+1)
!
    else if (option.eq.'CHAR_MECA_FLUX_F') then
!         WRITE(6,*)'OPTION',OPTION
        iopt = 2
        call jevech('PFLUXF', 'L', ifluxf)
        call jevech('PTEMPSR', 'L', itemps)
        deltat = zr(itemps+1)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itemps)
    else if (option.eq.'CHAR_MECA_PRES_R') then
!         WRITE(6,*)'OPTION',OPTION
        iopt = 3
        call jevech('PPRESSR', 'L', ipres)
!
    else if (option.eq.'CHAR_MECA_PRES_F') then
!         WRITE(6,*)'OPTION',OPTION
        iopt = 4
        call jevech('PPRESSF', 'L', ipresf)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itemps)
    else if (option.eq.'CHAR_MECA_FR2D3D') then
!         WRITE(6,*)'OPTION',OPTION
        iopt = 5
        call jevech('PFR2D3D', 'L', iforc)
    endif
!
    idfdx = ivf + npi * nno
    idfdy = idfdx + 1
!
! --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do 20 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
20  end do
!
! ======================================================================
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
! ======================================================================
    do 100 ipg = 1, npi
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
        ldec2 = (ipg-1)*nno2
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
! --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sx( i,j)
                ny = ny + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sy( i,j)
                nz = nz + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sz( i,j)
102          continue
!
        jac = sqrt(nx*nx+ny*ny+nz*nz)
!
! ======================================================================
! OPTIONS CHAR_MECA_FLUX_R ET CHAR_MECA_FLUX_F (INTERPOLATION LINEAIRE)
! ======================================================================
!
        if (iopt .eq. 1 .or. iopt .eq. 2) then
!
! ======================================================================
! --- SI MODELISATION = SUSHI HH2 AVEC OU SANS VOISINAGE
!
            if (nomte(1:10) .eq. 'ZHH2_FACE9') then
!
!
! --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
! --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
!     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
!
                napre1=0
                napre2=1
                nflux =2
!
                if (iopt .eq. 1) then
!
! ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
! ---   ET FLUTH LE FLUX THERMIQUE
!
                    flu1 = zr((iflux)+(ipg-1)*nflux+napre1 )
                    flu2 = zr((iflux)+(ipg-1)*nflux+napre2 )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 300 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec+i-1)
300                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 4, nompar, valpar,&
                                flu2, iret)
!
                endif
!
!
!
                do 301 i = 1, nno2
!
                    zr(ires) = zr(ires) - zr(ipoids+ipg-1) * flu1 * zr(ivf2+ldec2+i-1) * jac
!
                    zr(ires+1) = zr(ires+1) - zr(ipoids+ipg-1) * flu2 * zr(ivf2+ldec2+i-1) * jac
!
301              continue
!
!             WRITE(6,*)' ICI4'
            endif
!
! ======================================================================
! --- SI MODELISATION = THHM OU THH
!
            if (nomte(1:4) .eq. 'THHM' .or. nomte(1:4) .eq. 'THH_' .or. nomte (1:4) .eq.&
                'THH2') then
!
! --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
! --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
!     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
!
                napre1=0
                napre2=1
                natemp=2
!
                if (iopt .eq. 1) then
!
! ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
! ---   ET FLUTH LE FLUX THERMIQUE
!
                    flu1 = zr((iflux)+(ipg-1)*3+napre1 )
                    flu2 = zr((iflux)+(ipg-1)*3+napre2 )
                    fluth = zr((iflux)+(ipg-1)*3+natemp )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 104 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec+i-1)
104                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 4, nompar, valpar,&
                                flu2, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 4, nompar, valpar,&
                                fluth, iret)
!
                endif
!
                if (nomte(1:4) .eq. 'THHM' .or. nomte(1:5) .eq. 'THH2M') then
                    do 120 i = 1, nno2
                        l = 6 * (i-1) -1
!
                        zr(ires+l+4) = zr(ires+l+4) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+&
                                       &ldec2+i-1) * jac
                        zr(ires+l+5) = zr(ires+l+5) - zr(ipoids+ipg-1) * deltat * flu2 * zr(ivf2+&
                                       &ldec2+i-1) * jac
                        zr(ires+l+6) = zr(ires+l+6) - zr(ipoids+ipg-1) * deltat * fluth * zr(ivf2&
                                       &+ldec2+i-1) * jac
120                  continue
!
                else
                    do 127 i = 1, nno2
                        l = 3 * (i-1) -1
!
                        zr(ires+l+1) = zr(ires+l+1) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+&
                                       &ldec2+i-1) * jac
                        zr(ires+l+2) = zr(ires+l+2) - zr(ipoids+ipg-1) * deltat * flu2 * zr(ivf2+&
                                       &ldec2+i-1) * jac
                        zr(ires+l+3) = zr(ires+l+3) - zr(ipoids+ipg-1) * deltat * fluth * zr(ivf2&
                                       &+ldec2+i-1) * jac
127                  continue
                endif
!
            endif
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
!
            if (nomte(1:3) .eq. 'HH2' .or. nomte(1:3) .eq. 'HH_') then
!
! --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
! --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
!     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
!
                napre1=0
                napre2=1
!
                if (iopt .eq. 1) then
!
! ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
!
                    flu1 = zr((iflux)+(ipg-1)*2+napre1 )
                    flu2 = zr((iflux)+(ipg-1)*2+napre2 )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 116 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec+i-1)
116                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 4, nompar, valpar,&
                                flu2, iret)
!
                endif
!
                do 128 i = 1, nno2
                    l = 2 * (i-1) -1
!
                    zr(ires+l+1) = zr(ires+l+1) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+ldec&
                                   &2+i-1) * jac
                    zr(ires+l+2) = zr(ires+l+2) - zr(ipoids+ipg-1) * deltat * flu2 * zr(ivf2+ldec&
                                   &2+i-1) * jac
128              continue
!
            endif
!
!
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
!
!
!
!
! ======================================================================
! --- SI MODELISATION = THV
!
            if (nomte(1:4) .eq. 'THV_') then
!
! --- NAPRE1,NATEMP SONT MIS EN PLACE
! --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
!     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
!
                napre1=0
                natemp=1
!
                if (iopt .eq. 1) then
!
! ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
! ---   ET FLUTH LE FLUX THERMIQUE
!
                    flu1 = zr((iflux)+(ipg-1)*2+napre1 )
                    fluth = zr((iflux)+(ipg-1)*2+natemp )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 204 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec2+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec2+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec2+i-1)
204                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 4, nompar, valpar,&
                                flu2, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 4, nompar, valpar,&
                                fluth, iret)
!
                endif
!
                do 205 i = 1, nno2
                    l = 2 * (i-1) -1
!
                    zr(ires+l+1) = zr(ires+l+1) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+ldec&
                                   &2+i-1) * jac
                    zr(ires+l+2) = zr(ires+l+2) - zr(ipoids+ipg-1) * deltat * fluth * zr(ivf2+lde&
                                   &c2+i-1) * jac
205              continue
            endif
!
! ======================================================================
! SI MODELISATION = HM
!
            if (nomte(1:2) .eq. 'HM') then
!
                napre1=0
!
                if (iopt .eq. 1) then
!
! ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
!
                    flu1 = zr((iflux)+(ipg-1)+napre1 )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 106 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec2+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec2+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec2+i-1)
106                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
!
                endif
                do 122 i = 1, nno2
                    l = 4 * (i-1) -1
                    zr(ires+l+4) = zr(ires+l+4) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+ldec&
                                   &2+i-1) * jac
!
122              continue
!
            endif
! ======================================================================
! SI MODELISATION = HHM
!
            if (nomte(1:3) .eq. 'HHM' .or. nomte(1:4) .eq. 'HH2M') then
!
                napre1=0
                napre2=1
!
                if (iopt .eq. 1) then
!
! ---    FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---    FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
!
                    flu1 = zr((iflux)+(ipg-1)*2+napre1 )
                    flu2 = zr((iflux)+(ipg-1)*2+napre2 )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 105 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec2+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec2+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec2+i-1)
105                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 4, nompar, valpar,&
                                flu2, iret)
!
                endif
!
                do 121 i = 1, nno2
                    l = 5 * (i-1) -1
!
                    zr(ires+l+4) = zr(ires+l+4) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+ldec&
                                   &2+i-1) * jac
                    zr(ires+l+5) = zr(ires+l+5) - zr(ipoids+ipg-1) * deltat * flu2 * zr(ivf2+ldec&
                                   &2+i-1) * jac
121              continue
!
            endif
! ======================================================================
! SI MODELISATION = THM
!
            if (nomte(1:3) .eq. 'THM') then
!
                napre1=0
                natemp=1
!
                if (iopt .eq. 1) then
!
! ---    FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
! ---    ET FLUTH LA THERMIQUE
!
                    flu1 = zr((iflux)+(ipg-1)*2+napre1 )
                    fluth = zr((iflux)+(ipg-1)*2+natemp )
!
                else if (iopt.eq.2) then
                    x = 0.d0
                    y = 0.d0
                    z = 0.d0
                    do 115 i = 1, nno2
                        x = x + zr(igeom+3*i-3) * zr(ivf2+ldec2+i-1)
                        y = y + zr(igeom+3*i-2) * zr(ivf2+ldec2+i-1)
                        z = z + zr(igeom+3*i-1) * zr(ivf2+ldec2+i-1)
115                  continue
                    valpar(1) = x
                    valpar(2) = y
                    valpar(3) = z
!
                    call fointe('FM', zk8(ifluxf+napre1), 4, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 4, nompar, valpar,&
                                fluth, iret)
!
                endif
!
                do 131 i = 1, nno2
                    l = 5 * (i-1) -1
!
                    zr(ires+l+4) = zr(ires+l+4) - zr(ipoids+ipg-1) * deltat * flu1 * zr(ivf2+ldec&
                                   &2+i-1) * jac
                    zr(ires+l+5) = zr(ires+l+5) - zr(ipoids+ipg-1) * deltat * fluth * zr(ivf2+lde&
                                   &c2+i-1) * jac
131              continue
            endif
!
! ======================================================================
!        --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ---
! INTERPOLATION QUADRATIQUE
! ======================================================================
        else if ((iopt.eq.3) .or. (iopt.eq.4)) then
            if (iopt .eq. 3) then
                pres = 0.d0
                do 107 i = 1, nno
                    pres = pres + zr(ipres+i-1)*zr(ivf+ldec+i-1)
107              continue
            else if (iopt.eq.4) then
                pres = 0.d0
                do 108 i = 1, nno
                    valpar(1) = zr(igeom+3*i-3)
                    valpar(2) = zr(igeom+3*i-2)
                    valpar(3) = zr(igeom+3*i-1)
                    call fointe('FM', zk8(ipresf), 4, nompar, valpar,&
                                presf, iret)
                    pres = pres + presf*zr(ivf+ldec+i-1)
108              continue
            endif
!
            do 110 i = 1, nnos
                l = ndlno * (i-1) -1
                zr(ires+l+1) = zr(ires+l+1) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i-1) * nx
                zr(ires+l+2) = zr(ires+l+2) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i-1) * ny
                zr(ires+l+3) = zr(ires+l+3) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i-1) * nz
110          continue
            do 111 i = 1, (nno-nnos)
                l = ndlno*nnos + ndlnm*(i-1) -1
                zr(ires+l+1) = zr(ires+l+1) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i+nnos-1) * n&
                               &x
                zr(ires+l+2) = zr(ires+l+2) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i+nnos-1) * n&
                               &y
                zr(ires+l+3) = zr(ires+l+3) - zr(ipoids+ipg-1) * pres * zr(ivf+ldec+i+nnos-1) * n&
                               &z
111          continue
!
! ======================================================================
!        --- OPTION CHAR_MECA_FR2D3D : FORCE_FACE ---
! ======================================================================
!
        else if (iopt.eq.5) then
            fx = 0.0d0
            fy = 0.0d0
            fz = 0.0d0
            do 124 i = 1, nno
                fx = fx + zr(iforc-1+3*(i-1)+1)*zr(ivf+ldec+i-1)
                fy = fy + zr(iforc-1+3*(i-1)+2)*zr(ivf+ldec+i-1)
                fz = fz + zr(iforc-1+3*(i-1)+3)*zr(ivf+ldec+i-1)
124          continue
            do 123 i = 1, nno
                l = ndlno * (i-1) -1
                zr(ires+l+1) = zr(ires+l+1) + zr(ipoids+ipg-1)*fx*zr( ivf+ldec+i-1)*jac
                zr(ires+l+2) = zr(ires+l+2) + zr(ipoids+ipg-1)*fy*zr( ivf+ldec+i-1)*jac
                zr(ires+l+3) = zr(ires+l+3) + zr(ipoids+ipg-1)*fz*zr( ivf+ldec+i-1)*jac
123          continue
            do 125 i = 1, nno
                l = ndlno*nnos+ndlnm * (i-1) -1
                zr(ires+l+1) = zr(ires+l+1) + zr(ipoids+ipg-1)*fx*zr( ivf+ldec+i+nnos-1)*jac
                zr(ires+l+2) = zr(ires+l+2) + zr(ipoids+ipg-1)*fy*zr( ivf+ldec+i+nnos-1)*jac
                zr(ires+l+3) = zr(ires+l+3) + zr(ipoids+ipg-1)*fz*zr( ivf+ldec+i+nnos-1)*jac
125          continue
        endif
!
100  end do
!-----------------------------------------------------------------------
end subroutine
