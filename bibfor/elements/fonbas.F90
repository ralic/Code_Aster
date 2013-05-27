subroutine fonbas(noma, basfon, fontyp, fonfis, nbnoff,&
                  basloc, lnno, ltno)
!
    implicit none
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/cnscre.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nbnoff
    character(len=8) :: noma
    character(len=19) :: basfon, basloc, fontyp, lnno, ltno
    character(len=24) :: fonfis
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
!
! FONCTION REALISEE:
!
!     CALCUL :
!        - DE LA BASE LOCALE EN CHAQUE NOEUD DU MAILLAGE
!        - DES LEVEL-SETS    EN CHAQUE NOEUD DU MAILLAGE
!
!
!     ENTREES:
!        NOMA   : NOM DU MAILLAGE
!        BASFON : BASE AUX NOEUDS DU FOND DE FISSURE
!        FONTYP : TYPE DU FOND DE FISSURE
!        FONFIS : COORDONNEES DES NOEUDS DU FOND DE FISSURE
!        NBNOFF : NOMBRE DE NOEUDS AU FOND DE FISSURE
!     SORTIES:
!        BASLOC : BASE LOCALE EN CHAQUE NOEUD DU MAILLAGE
!        LTNO   : LEVEL-SETS TANGENTS EN CHAQUE NOEUD DU MAILLAGE
!        LNNO   : LEVLE-SETS NORMAUX EN CHAQUE NOEUD DU MAILLAGE
!-----------------------------------------------------------------------
!
    integer :: ibid, ifon, indica, indicb, ina, inb, ino
    integer :: iret, iseg, jbas, jcoor
    integer :: jgsl, jgsv, jlnsv, jlnsl, jltsv, jltsl, jtyp
    integer :: k, nbno, ndim, nseg
    real(kind=8) :: d, dmin, eps, norm2, s, sn, xln, xlt
    real(kind=8) :: xa, ya, za, xb, yb, zb, xm, ym, zm
    real(kind=8) :: xab, yab, zab, xam, yam, zam, xnm, ynm, znm
    real(kind=8) :: n(3), nm(3), vdira(3), vnora(3), vdirb(3), vnorb(3)
    real(kind=8) :: vdirn(3), vnorn(3)
    character(len=8) :: k8b, licmp(9), typfon
    character(len=16) :: casfon
    character(len=19) :: cnsbas, cnsln, cnslt
!
    data licmp / 'X1','X2','X3',&
     &             'X4','X5','X6',&
     &             'X7','X8','X9'/
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     ------------------------------------------------------------------
!     INITIALISATIONS
!     ------------------------------------------------------------------
!
!     RECUPERATION DES INFORMATIONS RELATIVES AU MAILLAGE
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8b, iret)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     NSEG : NOMBRE DE "SEGMENTS" DU FOND A TRAITER
    if (ndim .eq. 2) then
        nseg = 1
        casfon = ' '
    else if (ndim.eq.3) then
        call jeveuo(fontyp, 'L', jtyp)
        typfon = zk8(jtyp)
        casfon = 'LINEAIRE'
        nseg = nbnoff-1
!       CAS QUADRATIQUE
        if (typfon .eq. 'NOE3' .or. typfon .eq. 'SEG3') then
            casfon = 'QUADRATIQUE'
            nseg = (nbnoff-1)/2
        endif
    endif
!
!     INITIALISATION DES CHAMPS SIMPLES DES LEVEL-SETS
    cnslt = '&&FONBAS.CNSLT'
    cnsln = '&&FONBAS.CNSLN'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnslt)
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsln)
!
    call jeveuo(cnslt//'.CNSV', 'E', jltsv)
    call jeveuo(cnslt//'.CNSL', 'E', jltsl)
    call jeveuo(cnsln//'.CNSV', 'E', jlnsv)
    call jeveuo(cnsln//'.CNSL', 'E', jlnsl)
!
!     INITIALISATION DU CHAMP SIMPLE DE LA BASE LOCALE
    cnsbas = '&&FONBAS.CNSBAS'
    call cnscre(noma, 'NEUT_R', ndim*3, licmp, 'V',&
                cnsbas)
    call jeveuo(cnsbas//'.CNSV', 'E', jgsv)
    call jeveuo(cnsbas//'.CNSL', 'E', jgsl)
!
    call jeveuo(fonfis, 'L', ifon)
    call jeveuo(basfon, 'L', jbas)
!
!     ------------------------------------------------------------------
!                BOUCLE SUR LES NOEUDS DU MAILLAGE
!     ------------------------------------------------------------------
!
    eps = 1.d-12
    do 10 ino = 1, nbno
!
!       COORD DU NOEUD M DU MAILLAGE
        xm = zr(jcoor+(ino-1)*3+1-1)
        ym = zr(jcoor+(ino-1)*3+2-1)
        zm = zr(jcoor+(ino-1)*3+3-1)
!
!       CAS 2D : LE PROJETE EST TRIVIAL !
        if (ndim .eq. 2) then
!
!         COORD PT N
            n(1) = zr(ifon)
            n(2) = zr(ifon+1)
            n(3) = 0.d0
!
!         VECTEUR NM
            nm(1) = xm-n(1)
            nm(2) = ym-n(2)
!
!         STOCKAGE DES VECTEURS DE LA BASE
            do 110 k = 1, ndim
                zr(jgsv-1+3*ndim*(ino-1)+k) = n(k)
                zl(jgsl-1+3*ndim*(ino-1)+k) = .true.
                zr(jgsv-1+3*ndim*(ino-1)+k+2) = zr(jbas-1+k)
                zl(jgsl-1+3*ndim*(ino-1)+k+2) = .true.
                zr(jgsv-1+3*ndim*(ino-1)+k+4) = zr(jbas-1+k+ndim)
                zl(jgsl-1+3*ndim*(ino-1)+k+4) = .true.
110          continue
!
!         STOCKAGE DES LEVEL-SETS
            zr(jlnsv-1+(ino-1)+1)=nm(1)*zr(jbas-1+1)+nm(2)*zr(jbas-1+&
            2)
            zr(jltsv-1+(ino-1)+1)=nm(1)*zr(jbas-1+3)+nm(2)*zr(jbas-1+&
            4)
            zl(jlnsl-1+(ino-1)+1)=.true.
            zl(jltsl-1+(ino-1)+1)=.true.
!
!       CAS 3D : RECHERCHE DU PROJETE PUIS STOCKAGE DES VECTEURS
        else if (ndim.eq.3) then
!
!         RECHERCHE DU PROJETE DE INO SUR LE FOND DE FISSURE
!         --------------------------------------------------
            dmin = r8maem()
!
!         BOUCLE SUR LES "SEGMENTS" DU FOND DE FISSURE
            do 100 iseg = 1, nseg
!
                if (casfon .eq. 'LINEAIRE') then
                    ina = iseg
                    inb = iseg+1
                else if (casfon.eq.'QUADRATIQUE') then
                    ina = 2*iseg-1
                    inb = 2*iseg+1
                endif
!
!           COORD DES POINTS A ET B, EXTREMITES DU SEGMENT ISEG
                xa = zr(ifon-1+4*(ina-1)+1)
                ya = zr(ifon-1+4*(ina-1)+2)
                za = zr(ifon-1+4*(ina-1)+3)
                xb = zr(ifon-1+4*(inb-1)+1)
                yb = zr(ifon-1+4*(inb-1)+2)
                zb = zr(ifon-1+4*(inb-1)+3)
!
!           VECTEUR AB ET AM
                xab = xb-xa
                yab = yb-ya
                zab = zb-za
                xam = xm-xa
                yam = ym-ya
                zam = zm-za
!
!           PARAM S (PRODUIT SCALAIRE...)
                s = xab*xam + yab*yam + zab*zam
                norm2 = xab*xab + yab*yab + zab*zab
                s = s/norm2
!
!           SI N EN DEHORS DU SEGMENT AB
                if ((s-1) .ge. eps) s = 1.d0
                if (s .le. eps) s = 0.d0
!
!           COORD DU PROJETE DE M SUR ISEG: N
                xnm = xm - (s*xab+xa)
                ynm = ym - (s*yab+ya)
                znm = zm - (s*zab+za)
!
!           DISTANCE MN
                d = sqrt(xnm*xnm + ynm*ynm + znm*znm)
!
                if (d .lt. (dmin*(1-abs(r8prem())*100) )) then
                    dmin = d
                    sn = s
                    indica = ina
                    indicb = inb
!
                    n(1) = s*xab+xa
                    n(2) = s*yab+ya
                    n(3) = s*zab+za
                endif
!
100          continue
!
!         CALCUL DES VECTEURS DE LA BASE LOCALE AU POINT PROJETE
!         ------------------------------------------------------
!
            nm(1) = xm-n(1)
            nm(2) = ym-n(2)
            nm(3) = zm-n(3)
!
            do 200 k = 1, ndim
!
                vnora(k) = zr(jbas-1+6*(indica-1)+k)
                vdira(k) = zr(jbas-1+6*(indica-1)+k+ndim)
                vnorb(k) = zr(jbas-1+6*(indicb-1)+k)
                vdirb(k) = zr(jbas-1+6*(indicb-1)+k+ndim)
                vnorn(k) = sn*vnorb(k)+(1-sn)*vnora(k)
                vdirn(k) = sn*vdirb(k)+(1-sn)*vdira(k)
!
!           STOCKAGE DU PROJETE ET DES GRADIENTS
                zr(jgsv-1+3*ndim*(ino-1)+k) = n(k)
                zl(jgsl-1+3*ndim*(ino-1)+k) = .true.
                zr(jgsv-1+3*ndim*(ino-1)+k+3) = vdirn(k)
                zl(jgsl-1+3*ndim*(ino-1)+k+3) = .true.
                zr(jgsv-1+3*ndim*(ino-1)+k+6) = vnorn(k)
                zl(jgsl-1+3*ndim*(ino-1)+k+6) = .true.
!
200          continue
!
!         STOCKAGE DES LEVEL-SETS
            xln = nm(1)*vnorn(1)+nm(2)*vnorn(2)+nm(3)*vnorn(3)
            xlt = nm(1)*vdirn(1)+nm(2)*vdirn(2)+nm(3)*vdirn(3)
            zr(jlnsv-1+(ino-1)+1) = xln
            zr(jltsv-1+(ino-1)+1) = xlt
!
            zl(jlnsl-1+(ino-1)+1) = .true.
            zl(jltsl-1+(ino-1)+1) = .true.
!
!       CAS NI 2D NI 3D
        else
!
            call assert(.false.)
!
        endif
!
10  end do
!
!
! --- CREATION DES CHAM_NO
!
!     ENREGISTREMENT DE .LTNO, .LNNO ET .BASLOC DANS LA SD FOND_FISS
    call cnscno(cnslt, ' ', 'NON', 'G', ltno,&
                'F', ibid)
    call cnscno(cnsln, ' ', 'NON', 'G', lnno,&
                'F', ibid)
    call cnscno(cnsbas, ' ', 'NON', 'G', basloc,&
                'F', ibid)
!
!
!     MENAGE
    call detrsd('CHAM_NO_S', cnsln)
    call detrsd('CHAM_NO_S', cnslt)
    call detrsd('CHAM_NO_S', cnsbas)
!
    call jedema()
end subroutine
