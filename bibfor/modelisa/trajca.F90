subroutine trajca(tablca, mailla, icabl, nbnoca, xnoca,&
                  ynoca, znoca, comima, gromai)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : INTERPOLATION DE LA TRAJECTOIRE D'UN CABLE
!  -----------   APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!                EN SORTIE ON COMPLETE LA TABLE RESULTAT
!                LES LIGNES COMPLETEES CORRESPONDENT AU DERNIER CABLE
!                LES CASES RENSEIGNEES CORRESPONDENT AUX PARAMETRES
!                <ABSC_CURV> ET <ALPHA>
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : XNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ABSCISSES X DES NOEUDS APPARTENANT AUX CABLES
!  IN     : YNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    ORDONNEES Y DES NOEUDS APPARTENANT AUX CABLES
!  IN     : ZNOCA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR DE REELS POUR STOCKAGE DES
!                    COTES Z DES NOEUDS APPARTENANT AUX CABLES
!  IN     : GROMAI   CHARACTER*24
!                    NOM DU VECTEUR CONTENANT LES PLUS GRANDS
!                    DIAMETRES DES MAILLES DE BETON SELON LES
!                    DIRECTIONS X, Y ET Z
!  OUT    : COMIMA : CHARACTER*24
!                    NOM DU VECTEUR CONTENANT LES 6 COORDONNEES
!                    EXTREME QUI CONSTITUENT LE PAVE DANS LEQUEL
!                    EST CONTENU LE CALBLE
!
!  N.B. LES VECTEURS XNOCA, YNOCA ET ZNOCA SONT COMPLETES A CHAQUE
!       PASSAGE DANS LA ROUTINE TRAJCA : REAJUSTEMENT DE LEUR DIMENSION
!       PUIS REMPLISSAGE DES DERNIERS SOUS-BLOCS ALLOUES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/splin1.h"
#include "asterfort/splin2.h"
#include "asterfort/spline.h"
#include "asterfort/tbajli.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: mailla
    character(len=19) :: xnoca, ynoca, znoca, tablca
    character(len=24) :: comima, gromai
    integer :: icabl, nbnoca(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: ibid, idecal, ino, ipara, iret, isub,  jalph, jcoor
    integer :: jnoca, jtblp, jtbnp,    jx, jy, jz, nblign, nbno
    integer :: nbpara, numnoe, icmima,  nbvar, nbvar2, svar, vali(3)
    real(kind=8) :: absc, alpha, corde, alphcu, d1m, d1p, dcp, d1x, d1x1
    real(kind=8) :: d1xn, d1y, d1y1, d1yn, d1z, d1z1, d1zn, d2x, d2y, d2z, dc
    real(kind=8) :: dc1, dcn, det1, det2, det3, du, dx, dy, dz, normv2
    real(kind=8) :: valpar(2), dx1, dy1, dz1, psc, du1
    real(kind=8) :: xmin, xmax, ymin, ymax, zmin, zmax, rr
    complex(kind=8) :: cbid
    character(len=3) :: k3b
    character(len=24) :: coorno, nonoca, nonoma
    logical :: lsplin
!
    integer :: nbsub, jgmai
    parameter    (nbsub=5)
    character(len=24) :: param(2), parcr
    real(kind=8), pointer :: absc_curv(:) => null()
    real(kind=8), pointer :: alpha_disc(:) => null()
    real(kind=8), pointer :: corde_cumu(:) => null()
    real(kind=8), pointer :: vd2x(:) => null()
    real(kind=8), pointer :: vd2y(:) => null()
    real(kind=8), pointer :: vd2z(:) => null()
    data          param /'ABSC_CURV               ',&
     &                     'ALPHA                   '/
    data          parcr /'NOEUD_CABLE             '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    cbid=(0.d0,0.d0)
    ibid=0
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   CREATION DES OBJETS DE TRAVAIL
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nbno = nbnoca(icabl)
    AS_ALLOCATE(vr=corde_cumu, size=nbno)
    AS_ALLOCATE(vr=vd2x, size=nbno)
    AS_ALLOCATE(vr=vd2y, size=nbno)
    AS_ALLOCATE(vr=vd2z, size=nbno)
    AS_ALLOCATE(vr=absc_curv, size=nbno)
    call wkvect('&&TRAJCA.ALPHA', 'V V R', nbno, jalph)
    AS_ALLOCATE(vr=alpha_disc, size=nbno)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   RECUPERATION DES COORDONNEES DES NOEUDS DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call jeveuo(tablca//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp)
    nblign = zi(jtbnp+1)
    call jeveuo(tablca//'.TBLP', 'L', jtblp)
    do 10 ipara = 1, nbpara
        if (zk24(jtblp+4*(ipara-1)) .eq. parcr) then
            nonoca = zk24(jtblp+4*(ipara-1)+2)
            call jeveuo(nonoca, 'L', jnoca)
            goto 20
        endif
10  end do
!
20  continue
    idecal = nblign - nbno
!
    call jeecra(xnoca, 'LONUTI', nblign)
    call jeveuo(xnoca, 'E', jx)
    call jeecra(ynoca, 'LONUTI', nblign)
    call jeveuo(ynoca, 'E', jy)
    call jeecra(znoca, 'LONUTI', nblign)
    call jeveuo(znoca, 'E', jz)
!
    call jeveuo(comima, 'E', icmima)
    xmin=r8maem()
    ymin=r8maem()
    zmin=r8maem()
    xmax=-r8maem()
    ymax=-r8maem()
    zmax=-r8maem()
    nonoma = mailla//'.NOMNOE'
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    do 30 ino = 1, nbno
        call jenonu(jexnom(nonoma, zk8(jnoca+idecal+ino-1)), numnoe)
        zr(jx+idecal+ino-1) = zr(jcoor+3*(numnoe-1) )
        zr(jy+idecal+ino-1) = zr(jcoor+3*(numnoe-1)+1)
        zr(jz+idecal+ino-1) = zr(jcoor+3*(numnoe-1)+2)
!
!        RECHERCHE DES COORDONNEES EXTREMES
        if (zr(jx+idecal+ino-1) .lt. xmin) xmin=zr(jx+idecal+ino-1)
        if (zr(jy+idecal+ino-1) .lt. ymin) ymin=zr(jy+idecal+ino-1)
        if (zr(jz+idecal+ino-1) .lt. zmin) zmin=zr(jz+idecal+ino-1)
        if (zr(jx+idecal+ino-1) .gt. xmax) xmax=zr(jx+idecal+ino-1)
        if (zr(jy+idecal+ino-1) .gt. ymax) ymax=zr(jy+idecal+ino-1)
        if (zr(jz+idecal+ino-1) .gt. zmax) zmax=zr(jz+idecal+ino-1)
30  end do
!     CONSTRUCTION DES 6 PLANS DEFINISSANT DE PAVE CONTENANT LE CABLE
    call jeveuo(gromai, 'L', jgmai)
    rr = zr(jgmai)
    zr(icmima-1+1)=xmin-rr/2.d0
    zr(icmima-1+2)=xmax+rr/2.d0
    rr = zr(jgmai+1)
    zr(icmima-1+3)=ymin-rr/2.d0
    zr(icmima-1+4)=ymax+rr/2.d0
    rr = zr(jgmai+2)
    zr(icmima-1+5)=zmin-rr/2.d0
    zr(icmima-1+6)=zmax+rr/2.d0
!
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   CALCUL DU PARAMETRE CORDE CUMULEE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    corde_cumu(1) = 0.0d0
    alpha_disc(1) = 0.0d0
    alphcu = 0.0d0
!
!.... N.B. LE PASSAGE PREALABLE DANS LA ROUTINE TOPOCA GARANTIT NBNO > 2
!
    do 40 ino = 2, nbno
        dx = zr(jx+idecal+ino-1) - zr(jx+idecal+ino-2)
        dy = zr(jy+idecal+ino-1) - zr(jy+idecal+ino-2)
        dz = zr(jz+idecal+ino-1) - zr(jz+idecal+ino-2)
        du = dble ( sqrt ( dx * dx + dy * dy + dz * dz ) )
        if (du .eq. 0.0d0) then
            write(k3b,'(I3)') icabl
            call utmess('F', 'MODELISA7_59', sk=k3b)
        endif
        corde_cumu(ino) = corde_cumu(1+ino-2) + du
!
!        CALCUL DISCRET DES DEVIATIONS ANGULAIRES CUMULEES
!        UTILISE EN CAS D ECHEC DE L INTERPOLATION PAR SPLINE
        if (ino .lt. nbno) then
            dx1 = zr(jx+idecal+ino) - zr(jx+idecal+ino-1)
            dy1 = zr(jy+idecal+ino) - zr(jy+idecal+ino-1)
            dz1 = zr(jz+idecal+ino) - zr(jz+idecal+ino-1)
            du1 = dble ( sqrt ( dx1 * dx1 + dy1 * dy1 + dz1 * dz1 ) )
            psc = dx * dx1 + dy *dy1 + dz * dz1
            psc = abs(psc /(du * du1))
            alpha = trigom('ACOS',psc)
            alpha_disc(ino) = alphcu + alpha/2.d0
            alphcu = alphcu + alpha
        else
            alpha_disc(ino) = alphcu
        endif
!
40  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   INTERPOLATION SPLINE CUBIQUE DE LA TRAJECTOIRE DU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    dc1 = corde_cumu(1+1) - corde_cumu(1)
    dcn = corde_cumu(nbno) - corde_cumu(1+nbno-2)
!
! 4.1 INTERPOLATION DE LA COORDONNEE X
! ---
    d1x1 = ( zr(jx+idecal+1) - zr(jx+idecal) ) / dc1
    d1xn = ( zr(jx+idecal+nbno-1) - zr(jx+idecal+nbno-2) ) / dcn
!
    call spline(corde_cumu, zr(jx+idecal), nbno, d1x1, d1xn,&
                vd2x, iret)
!
! 4.2 INTERPOLATION DE LA COORDONNEE Y
! ---
    d1y1 = ( zr(jy+idecal+1) - zr(jy+idecal) ) / dc1
    d1yn = ( zr(jy+idecal+nbno-1) - zr(jy+idecal+nbno-2) ) / dcn
    call spline(corde_cumu, zr(jy+idecal), nbno, d1y1, d1yn,&
                vd2y, iret)
!
! 4.3 INTERPOLATION DE LA COORDONNEE Z
! ---
    d1z1 = ( zr(jz+idecal+1) - zr(jz+idecal) ) / dc1
    d1zn = ( zr(jz+idecal+nbno-1) - zr(jz+idecal+nbno-2) ) / dcn
    call spline(corde_cumu, zr(jz+idecal), nbno, d1z1, d1zn,&
                vd2z, iret)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4-BIS  CONTROLE DE L'INTERPOLATION SPLINE CUBIQUE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    lsplin = .true.
!
! 4-BIS .1 INTERPOLATION DE LA COORDONNEE X
! ---
!
!     CALCUL DU NOMBRE DE VARIATION DE LA DERIVEE PREMIERE
    nbvar = 0
    d1m = d1x1
    svar = 0
    do 45 ino = 2, nbno-1
        dcp = corde_cumu(ino+1) - corde_cumu(ino)
        d1p = ( zr(jx+idecal+ino) - zr(jx+idecal+ino-1) ) / dcp
        if (d1p .gt. d1m) then
            if (svar .eq. -1) nbvar = nbvar + 1
            svar = 1
        else if (d1p .lt. d1m) then
            if (svar .eq. 1) nbvar = nbvar + 1
            svar = -1
        endif
45  end do
!
!     CONTROLE DE LA REGULARITE DE LA DERIVEE SECONDE
    nbvar2 = 0
    do 46 ino = 2, nbno
        if (vd2x(ino)*vd2x(ino-1) .lt. 0.d0) nbvar2 = nbvar2+ 1
46  end do
!
    if (nbvar2 .ge. nbvar+10) then
        vali(1) = icabl
        vali(2) = nbvar2
        vali(3) = nbvar
        call utmess('I', 'MODELISA7_13', ni=3, vali=vali)
        lsplin = .false.
        goto 888
    endif
!
! 4-BIS .2 INTERPOLATION DE LA COORDONNEE Y
! ---
!
!     CALCUL DU NOMBRE DE VARIATION DE LA DERIVEE PREMIERE
    nbvar = 0
    d1m = d1y1
    svar = 0
    do 55 ino = 2, nbno-1
        dcp = corde_cumu(ino+1) - corde_cumu(ino)
        d1p = ( zr(jy+idecal+ino) - zr(jy+idecal+ino-1) ) / dcp
        if (d1p .gt. d1m) then
            if (svar .eq. -1) nbvar = nbvar + 1
            svar = 1
        else if (d1p .lt. d1m) then
            if (svar .eq. 1) nbvar = nbvar + 1
            svar = -1
        endif
55  end do
!
!     CONTROLE DE LA REGULARITE DE LA DERIVEE SECONDE
    nbvar2 = 0
    do 56 ino = 2, nbno
        if (vd2y(ino)*vd2y(ino-1) .lt. 0.d0) nbvar2 = nbvar2+ 1
56  end do
!
    if (nbvar2 .ge. nbvar+10) then
        vali(1) = icabl
        vali(2) = nbvar2
        vali(3) = nbvar
        call utmess('I', 'MODELISA7_13', ni=3, vali=vali)
        lsplin = .false.
        goto 888
    endif
!
! 4-BIS .3 INTERPOLATION DE LA COORDONNEE Z
! ---
!
!     CALCUL DU NOMBRE DE VARIATION DE LA DERIVEE PREMIERE
    nbvar = 0
    d1m = d1z1
    svar = 0
    do 65 ino = 2, nbno-1
        dcp = corde_cumu(ino+1) - corde_cumu(ino)
        d1p = ( zr(jz+idecal+ino) - zr(jz+idecal+ino-1) ) / dcp
        if (d1p .gt. d1m) then
            if (svar .eq. -1) nbvar = nbvar + 1
            svar = 1
        else if (d1p .lt. d1m) then
            if (svar .eq. 1) nbvar = nbvar + 1
            svar = -1
        endif
65  end do
!
!     CONTROLE DE LA REGULARITE DE LA DERIVEE SECONDE
    nbvar2 = 0
    do 66 ino = 2, nbno
        if (vd2z(ino)*vd2z(ino-1) .lt. 0.d0) nbvar2 = nbvar2+ 1
66  end do
!
    if (nbvar2 .ge. nbvar+10) then
        vali(1) = icabl
        vali(2) = nbvar2
        vali(3) = nbvar
        call utmess('I', 'MODELISA7_13', ni=3, vali=vali)
        lsplin = .false.
        goto 888
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 5   CALCULS DE L'ABSCISSE CURVILIGNE ET DE LA DEVIATION ANGULAIRE
!     CUMULEE LE LONG DU CABLE, PAR INTEGRATION NUMERIQUE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    absc_curv(1) = 0.0d0
    zr(jalph) = 0.0d0
!
    do 50 ino = 2, nbno
!
        corde = corde_cumu(1+ino-2)
        dc = ( corde_cumu(ino) - corde ) / dble ( nbsub )
!
!....... CONTRIBUTION DU PREMIER POINT
!
        call splin1(corde_cumu, zr(jx+idecal), vd2x, nbno, corde,&
                    d1x, iret)
        call splin1(corde_cumu, zr(jy+idecal), vd2y, nbno, corde,&
                    d1y, iret)
        call splin1(corde_cumu, zr(jz+idecal), vd2z, nbno, corde,&
                    d1z, iret)
        call splin2(corde_cumu, vd2x, nbno, corde, d2x,&
                    iret)
        call splin2(corde_cumu, vd2y, nbno, corde, d2y,&
                    iret)
        call splin2(corde_cumu, vd2z, nbno, corde, d2z,&
                    iret)
        normv2 = d1x * d1x + d1y * d1y + d1z * d1z
        if (normv2 .eq. 0.0d0) then
            write(k3b,'(I3)') icabl
            call utmess('F', 'MODELISA7_60', sk=k3b)
        endif
        absc = dble ( sqrt ( normv2 ) ) / 2.0d0
        det1 = d1y * d2z - d1z * d2y
        det2 = d1z * d2x - d1x * d2z
        det3 = d1x * d2y - d1y * d2x
        alpha = dble ( sqrt (det1*det1 + det2*det2 + det3*det3 )) / ( normv2 * 2.0d0)
!
!....... CONTRIBUTION DES POINTS INTERMEDIAIRES
!
        do 60 isub = 1, nbsub-1
            corde = corde + dc
            call splin1(corde_cumu, zr(jx+idecal), vd2x, nbno, corde,&
                        d1x, iret)
            call splin1(corde_cumu, zr(jy+idecal), vd2y, nbno, corde,&
                        d1y, iret)
            call splin1(corde_cumu, zr(jz+idecal), vd2z, nbno, corde,&
                        d1z, iret)
            call splin2(corde_cumu, vd2x, nbno, corde, d2x,&
                        iret)
            call splin2(corde_cumu, vd2y, nbno, corde, d2y,&
                        iret)
            call splin2(corde_cumu, vd2z, nbno, corde, d2z,&
                        iret)
            normv2 = d1x * d1x + d1y * d1y + d1z * d1z
            if (normv2 .eq. 0.0d0) then
                write(k3b,'(I3)') icabl
                call utmess('F', 'MODELISA7_60', sk=k3b)
            endif
            absc = absc + dble ( sqrt ( normv2 ) )
            det1 = d1y * d2z - d1z * d2y
            det2 = d1z * d2x - d1x * d2z
            det3 = d1x * d2y - d1y * d2x
            alpha = alpha + dble(sqrt(det1*det1+det2*det2+det3*det3)) / normv2
60      continue
!
!....... CONTRIBUTION DU DERNIER POINT
!
        corde = corde + dc
        call splin1(corde_cumu, zr(jx+idecal), vd2x, nbno, corde,&
                    d1x, iret)
        call splin1(corde_cumu, zr(jy+idecal), vd2y, nbno, corde,&
                    d1y, iret)
        call splin1(corde_cumu, zr(jz+idecal), vd2z, nbno, corde,&
                    d1z, iret)
        call splin2(corde_cumu, vd2x, nbno, corde, d2x,&
                    iret)
        call splin2(corde_cumu, vd2y, nbno, corde, d2y,&
                    iret)
        call splin2(corde_cumu, vd2z, nbno, corde, d2z,&
                    iret)
        normv2 = d1x * d1x + d1y * d1y + d1z * d1z
        if (normv2 .eq. 0.0d0) then
            write(k3b,'(I3)') icabl
            call utmess('F', 'MODELISA7_60', sk=k3b)
        endif
        absc = absc + dble ( sqrt ( normv2 ) ) / 2.0d0
        det1 = d1y * d2z - d1z * d2y
        det2 = d1z * d2x - d1x * d2z
        det3 = d1x * d2y - d1y * d2x
        alpha = alpha + dble( sqrt(det1*det1+det2*det2+det3*det3)) / ( normv2 * 2.0d0)
!
!....... ABSCISSE CURVILIGNE ET DEVIATION ANGULAIRE CUMULEE
!
        absc = absc * dc
        absc_curv(ino) = absc_curv(1+ino-2) + absc
        alpha = alpha * dc
        zr(jalph+ino-1) = zr(jalph+ino-2) + alpha
!
50  end do
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 6   MISE A JOUR DES OBJETS DE SORTIE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
888  continue
    if (lsplin) then
        do 70 ino = 1, nbno
            valpar(1) = absc_curv(ino)
            valpar(2) = zr(jalph+ino-1)
            call tbajli(tablca, 2, param, [ibid], valpar,&
                        [cbid], k3b, idecal+ ino)
70      continue
    else
        do 71 ino = 1, nbno
            valpar(1) = corde_cumu(ino)
            valpar(2) = alpha_disc(ino)
            call tbajli(tablca, 2, param, [ibid], valpar,&
                        [cbid], k3b, idecal+ ino)
71      continue
    endif
!
! --- MENAGE
!
    AS_DEALLOCATE(vr=corde_cumu)
    AS_DEALLOCATE(vr=vd2x)
    AS_DEALLOCATE(vr=vd2y)
    AS_DEALLOCATE(vr=vd2z)
    AS_DEALLOCATE(vr=absc_curv)
    call jedetr('&&TRAJCA.ALPHA')
    AS_DEALLOCATE(vr=alpha_disc)
!
    call jedema()
!
! --- FIN DE TRAJCA.
end subroutine
