subroutine op0056()
! aslint: disable=W1501
    implicit none
!.......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MULTI_COUCHE : COMMANDE DEFI_COQU_MULT
!     ROUTINE ASSOCIE A LA COMMANDE DECRIVANT UN MATERIAU MULTICOUCHE
!     COUCHE PAR COUCHE (EPAISSEUR,TYPE DU MATERIAU,PREMIERE DIRECTION
!                        D'ORTHOTROPIE)
!.......................................................................
! .                                                                    .
! .  - FONCTION REALISEE : REMPLISSAGE DE L'OBJET MULTIC//'.MATER.MECA'.
! .                     CONTENANT LE COMPORTEMENT HOMOGENEISEE ET LES
! .                        CARACTERISTIQUES MECANIQUE OU THERMIQUE     .
! .                        COUCHE PAR COUCHE DANS LE REPERE DE         .
! .                        REFERENCE                                   .
! .  - ARGUMENTS :                                                     .
! .                                                                    .
! .  - ROUTINES APPELEES :                                             .
! .    GETRES  GETFAC  GETVR8  GETVID  GETVIS  WKVECT  RCVALE          .
! .    JEDETR                                                          .
! .                                                                    .
! .  - OBJETS CREES :                                               .
! .    MULTIC//'.ELAS_COQMU.VALR'   MULTIC//'.THER_COQMU.VALR'
! .    MULTIC//'.ELAS_COQMU.VALC'   MULTIC//'.THER_COQMU.VALC'
! .    MULTIC//'.ELAS_COQMU.VALK'   MULTIC//'.THER_COQMU.VALK'
! .    MULTIC//'.MATERIAU.NOMRC'
!
!      REFERENCE : DHATT - BATOZ VOL 2 PP 238 - 243
!.......................................................................
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mgauss.h"
#include "asterfort/rcvale.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ifr, nbcou, icou, n, jepor, jrela, k, lonobj, jmate, jobme
    integer :: jobmc, i, nimpr, impr, nbres, j, nobj, jobth, jobtc, nbad, iret
    integer :: nv, n1
    real(kind=8) :: laml, lamt, lamn, cp, qt(31), valres(12)
    real(kind=8) :: epais, orien, eptot, epi, ordi, rhohom
    real(kind=8) :: el, et, nult, glt, qll, qtt, qlt, gln, gtn
    real(kind=8) :: q11, q12, q13, q22, q23, q33, qm(56)
    real(kind=8) :: m11, m12, m13, m21, m22, m23, m31, m32, m33
    real(kind=8) :: dl, dt, rho, g11, g22, g12
    real(kind=8) :: c, s, c2, s2, c3, s3, c4, s4, d11, d22, d12
    real(kind=8) :: hf(3, 3), hfinv(3, 3), hi(3, 3), ai(3, 3)
    real(kind=8) :: da1i(2, 2), idi(2, 2), ezd(2, 2)
    real(kind=8) :: xab(2, 2), c11i(2, 2), c11(2, 2), hti(2, 2), det
    real(kind=8) :: c11i1(2, 2), c11i2(2, 2), c11i3(2, 2), c11i4(2, 2)
    real(kind=8) :: x1, x2, x3, x4, xt, xc, yt, yc, slt
    real(kind=8) :: htiinv(2, 2), c11inv(2, 2)
    real(kind=8) :: hc11, hc22, hc12, k11, k22, k12
    real(kind=8) :: a11, a12, a13, a22, a23, a33
    real(kind=8) :: b11, b12, b13, b22, b23, b33
    real(kind=8) :: l11, l22, l12
    real(kind=8) :: rzkp, rzkm, dif3, dif4, dif5
    real(kind=8) :: r8bid, h, h2, h3, h4, epsi
    integer :: icodre(12)
    character(len=2) :: val
    character(len=6) :: k6
    character(len=24) :: valk(2)
    character(len=3) :: num
    character(len=8) :: k8b, multic, mater
    character(len=16) :: type, nomcmd, fichie, nomres(12)
    aster_logical :: elas, ther
    character(len=32), pointer :: nomrc(:) => null()
    parameter (nv=83)
!
    r8bid = 0.d0
    call jemarq()
    call infmaj()
!
    epsi=r8miem()
!
    call getres(multic, type, nomcmd)
    call getfac('COUCHE', nbcou)
!
!     --- VERIFICATION DES MATERIAUX ---
!
    elas = .false.
    ther = .false.
    do 20 icou = 1, nbcou
        call getvid('COUCHE', 'MATER', iocc=icou, scal=mater, nbret=n)
        call jeveuo(mater//'.MATERIAU.NOMRC ', 'L', vk32=nomrc)
        call jelira(mater//'.MATERIAU.NOMRC ', 'LONMAX', nbad)
        do 10 i = 1, nbad
            if (nomrc(i) .eq. 'ELAS_ORTH       ') then
                elas = .true.
            else if (nomrc(i).eq.'THER_ORTH       ') then
                ther = .true.
            else
                valk (1) = mater
                call utmess('F', 'MODELISA8_71', sk=valk(1))
            endif
 10     continue
 20 end do
    if (ther .and. elas) then
        valk (1) = 'MECANIQUE'
        valk (2) = 'THERMIQUE'
        call utmess('F', 'MODELISA8_72', nk=2, valk=valk)
    endif
!
    nimpr = 0
    call getfac('IMPRESSION', impr)
    if (impr .ne. 0) then
        nimpr = 1
        ifr = 0
        fichie = ' '
        call getvis('IMPRESSION', 'UNITE', iocc=1, scal=ifr, nbret=n1)
        if (.not. ulexis( ifr )) then
            call ulopen(ifr, ' ', fichie, 'NEW', 'O')
        endif
    endif
!
!     - PHASE D'EXECUTION, REMPLISSAGE DES OBJETS JEVEUX ------
!
    if (elas) then
!
!     ------ CARACTERISTIQUES MECANIQUES ------
!
        call wkvect('&&OP0056.EPOR', 'V V R', 3*nbcou, jepor)
        call wkvect(multic//'.MATERIAU.NOMRC ', 'G V K32', nbcou+2, jrela)
        zk32(jrela) = 'ELAS_COQMU      '
        lonobj = 56 + nv*nbcou
        call codent(1, 'D0', k6)
        call wkvect(multic//'.CPT.'//k6//'.VALK', 'G V K16', 2*lonobj, jmate)
        call jeecra(multic//'.CPT.'//k6//'.VALK', 'LONUTI', lonobj)
        call wkvect(multic//'.CPT.'//k6//'.VALR', 'G V R', lonobj, jobme)
        call jeecra(multic//'.CPT.'//k6//'.VALR', 'LONUTI', lonobj)
        call wkvect(multic//'.CPT.'//k6//'.VALC', 'G V C', lonobj, jobmc)
        call jeecra(multic//'.CPT.'//k6//'.VALC', 'LONUTI', 0)
        eptot = 0.d0
        do 30 i = 1, 56
            call codent(i, 'G', num)
            zk16(jmate+i-1) = 'HOM_'//num
 30     continue
        do 50 icou = 1, nbcou
            call getvr8('COUCHE', 'EPAIS', iocc=icou, scal=epais, nbret=n)
            call getvid('COUCHE', 'MATER', iocc=icou, scal=mater, nbret=n)
            call getvr8('COUCHE', 'ORIENTATION', iocc=icou, scal=orien, nbret=n)
            zk32(jrela+1+icou) = mater
            call codent(icou, 'G', num)
            do 40 i = 1, nv
                call codent(i, 'G', val)
                zk16(jmate+56+nv* (icou-1)+i-1) = 'C'//num//'_V'//val
 40         continue
            zr(jepor-1+3*icou-2) = epais
            zr(jepor-1+3*icou-1) = orien
            eptot = eptot + epais
 50     continue
        epais = -0.5d0*eptot
        rhohom = 0.d0
        do 60 i = 1, 56
            qm(i) = 0.d0
 60     continue
        iret = 0
        do 70 i = 1, nbcou
            nbres = 9
            nomres(1) = 'E_L'
            nomres(2) = 'E_T'
            nomres(3) = 'NU_LT'
            nomres(4) = 'G_LT'
            nomres(5) = 'ALPHA_L'
            nomres(6) = 'ALPHA_T'
            nomres(7) = 'RHO'
            nomres(8) = 'G_LN'
            nomres(9) = 'G_TN'
            nomres(10) = 'AMOR_ALPHA'
            nomres(11) = 'AMOR_BETA'
            nomres(12) = 'AMOR_HYST'
!         EN PRINCIPE G_LN = G_LT CF BATOZ
            k8b = ' '
            call rcvale(zk32(jrela+i+1) (1:8), 'ELAS_ORTH', 0, k8b, [r8bid],&
                        6, nomres, valres, icodre, 2)
            call rcvale(zk32(jrela+i+1) (1:8), 'ELAS_ORTH', 0, k8b, [r8bid],&
                        3, nomres(7), valres(7), icodre(7), 0)
            call rcvale(zk32(jrela+i+1) (1:8), 'ELAS_ORTH', 0, k8b, [r8bid],&
                        3, nomres(10), valres(10), icodre(10), 0)
            do j = 10,12
                if (icodre(j) .eq. 0 ) then
                    call utmess('A','MODELISA8_16',sk=nomres(j), si=i)
                endif
            enddo
            
            el = valres(1)
            et = valres(2)
            nult = valres(3)
            glt = valres(4)
            dl = valres(5)
            dt = valres(6)
            if (icodre(7) .ne. 0) then
                iret = iret + 1
                rho = 0.d0
            else
                rho = valres(7)
            endif
!           --- GLN NON DEFINI ==> GLN = GLT (ISOTROPIE TRANSVERSE) ---
            if (icodre(8) .ne. 0) then
                gln = glt
            else
                gln = valres(8)
            endif
!           --- GTN NON DEFINI ==> GRANDE RIGIDITE --------------------
            if (icodre(9) .ne. 0) then
                gtn = 1.d+6*glt
            else
                gtn = valres(9)
            endif
            if (el .lt. epsi) then
                qll = 0.d0
                qtt = et
            else
                qll = el/ (1.d0-nult*nult*et/el)
                qtt = qll*et/el
            endif
            qlt = qtt*nult
            orien = zr(jepor-1+3*i-1)
            c = cos(orien*r8dgrd())
            s = sin(orien*r8dgrd())
            c2 = c*c
            s2 = s*s
            c3 = c**3
            s3 = s**3
            c4 = c**4
            s4 = s**4
            epi = zr(jepor-1+3*i-2)
            epais = epais + epi
!         COTE MOYENNE DE LA COUCHE I
            zr(jepor-1+3*i) = epais - 0.5d0*epi
            ordi = zr(jepor-1+3*i)
            rhohom = rhohom + rho*epi/eptot
!          QIJ : MATRICE DE COMPORTEMENT H DANS LE REPERE UTILISATEUR
            q11 = c4*qll + s4*qtt + 2.d0*c2*s2*qlt + 4.d0*c2*s2*glt
            q22 = s4*qll + c4*qtt + 2.d0*c2*s2*qlt + 4.d0*c2*s2*glt
            q12 = c2*s2* (qll+qtt-4.d0*glt) + (s4+c4)*qlt
            q13 = c3*s*qll - c*s3*qtt + (c*s3-c3*s)* (qlt+2.d0*glt)
            q23 = c*s3*qll - c3*s*qtt - (c*s3-c3*s)* (qlt+2.d0*glt)
            q33 = c2*s2* (qll+qtt-2.d0*qlt) + (c2-s2)*(c2-s2)*glt
            
            

            
            
!         COEF DE DILATATION THERMIQUE REPERE UTILISATEUR
            d11 = c2*dl + s2*dt
            d22 = s2*dl + c2*dt
            d12 = 1.d0*c*s* (dl-dt)
!          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
            g11 = c2*gln + s2*gtn
            g22 = s2*gln + c2*gtn
            g12 = c*s* (gln-gtn)
!          T1T*HL*ALPHA
            m11 = (c2*qll+s2*qlt)*dl + (c2*qlt+s2*qtt)*dt
            m12 = (c*s*qll-s*c*qlt)*dl + (c*s*qlt-s*c*qtt)*dt
            m13 = 0.d0
            m21 = (c*s*qll-s*c*qlt)*dl + (c*s*qlt-s*c*qtt)*dt
            m22 = (s2*qll+c2*qlt)*dl + (s2*qlt+c2*qtt)*dt
            m23 = 0.d0
            m31 = 0.0
            m32 = 0.0
            m33 = 0.d0
!
!       LECTURE DES CRITERES
            nbres = 5
            nomres(1) = 'XT'
            nomres(2) = 'XC'
            nomres(3) = 'YT'
            nomres(4) = 'YC'
            nomres(5) = 'S_LT'
            k8b = ' '
            call rcvale(zk32(jrela+i+1) (1:8), 'ELAS_ORTH', 0, k8b, [r8bid],&
                        5, nomres, valres, icodre, 2)
            if (icodre(1) .ne. 0) then
                xt=r8vide()
            else
                xt = valres(1)
            endif
            if (icodre(2) .ne. 0) then
                xc=r8vide()
            else
                xc = valres(2)
            endif
            if (icodre(3) .ne. 0) then
                yt=r8vide()
            else
                yt = valres(3)
            endif
            if (icodre(4) .ne. 0) then
                yc=r8vide()
            else
                yc = valres(4)
            endif
            if (icodre(5) .ne. 0) then
                slt=r8vide()
            else
                slt = valres(5)
            endif
!
!         STOCKAGE DANS MULTIC//'.ELAS_COQMU.VALR'
!
!         EPAISSEUR DE LA COUCHE I
            zr(jobme+56+ (i-1)*nv) = epi
!         ANGLE ENTRE L'AXE L DE LA COUCHE I ET X DU REPERE UTILISATEUR
            zr(jobme+56+ (i-1)*nv+1) = orien
!         COTE MOYENNE DE LA COUCHE I
            zr(jobme+56+ (i-1)*nv+2) = ordi
!          QIJ : MATRICE DE COMPORTEMENT H DANS LE REPERE UTILISATEUR
            zr(jobme+56+ (i-1)*nv+3) = q11
            zr(jobme+56+ (i-1)*nv+4) = q12
            zr(jobme+56+ (i-1)*nv+5) = q13
            zr(jobme+56+ (i-1)*nv+6) = q22
            zr(jobme+56+ (i-1)*nv+7) = q23
            zr(jobme+56+ (i-1)*nv+8) = q33
!         COEF DE DILATATION THERMIQUE REPERE UTILISATEUR
            zr(jobme+56+ (i-1)*nv+9) = d11
            zr(jobme+56+ (i-1)*nv+10) = d22
            zr(jobme+56+ (i-1)*nv+11) = d12
!          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
            zr(jobme+56+ (i-1)*nv+12) = g11
            zr(jobme+56+ (i-1)*nv+13) = g22
            zr(jobme+56+ (i-1)*nv+14) = g12
!
!         HM = SOMME(I=1,NCOU)(EPI*H)
            qm(1) = qm(1) + q11*epi
            qm(2) = qm(2) + q12*epi
            qm(3) = qm(3) + q13*epi
            qm(4) = qm(4) + q22*epi
            qm(5) = qm(5) + q23*epi
            qm(6) = qm(6) + q33*epi
!         HMF = SOMME(I=1,NCOU)(EPI*ORDI*H)
            qm(7) = qm(7) + q11*epi*ordi
            qm(8) = qm(8) + q12*epi*ordi
            qm(9) = qm(9) + q13*epi*ordi
            qm(10) = qm(10) + q22*epi*ordi
            qm(11) = qm(11) + q23*epi*ordi
            qm(12) = qm(12) + q33*epi*ordi
!         HF = SOMME(I=1,NCOU)(ZI+1**3-ZI**3)*H/3)
            qm(13) = qm(13) + q11*epi* (ordi**2+epi**2/12.d0)
            qm(14) = qm(14) + q12*epi* (ordi**2+epi**2/12.d0)
            qm(15) = qm(15) + q13*epi* (ordi**2+epi**2/12.d0)
            qm(16) = qm(16) + q22*epi* (ordi**2+epi**2/12.d0)
            qm(17) = qm(17) + q23*epi* (ordi**2+epi**2/12.d0)
            qm(18) = qm(18) + q33*epi* (ordi**2+epi**2/12.d0)
!
            qm(30) = qm(30) + m11*epi
            qm(31) = qm(31) + m12*epi
            qm(32) = qm(32) + m13*epi
            qm(33) = qm(33) + m21*epi
            qm(34) = qm(34) + m22*epi
            qm(35) = qm(35) + m23*epi
            qm(36) = qm(36) + m31*epi
            qm(37) = qm(37) + m32*epi
            qm(38) = qm(38) + m33*epi
!
            qm(39) = qm(39) + m11*epi*ordi
            qm(40) = qm(40) + m12*epi*ordi
            qm(41) = qm(41) + m13*epi*ordi
            qm(42) = qm(42) + m21*epi*ordi
            qm(43) = qm(43) + m22*epi*ordi
            qm(44) = qm(44) + m23*epi*ordi
            qm(45) = qm(45) + m31*epi*ordi
            qm(46) = qm(46) + m32*epi*ordi
            qm(47) = qm(47) + m33*epi*ordi
            qm(48) = qm(48) + m11*epi* (ordi**2+epi**2/12.d0)
            qm(49) = qm(49) + m12*epi* (ordi**2+epi**2/12.d0)
            qm(50) = qm(50) + m13*epi* (ordi**2+epi**2/12.d0)
            qm(51) = qm(51) + m21*epi* (ordi**2+epi**2/12.d0)
            qm(52) = qm(52) + m22*epi* (ordi**2+epi**2/12.d0)
            qm(53) = qm(53) + m23*epi* (ordi**2+epi**2/12.d0)
            qm(54) = qm(54) + m31*epi* (ordi**2+epi**2/12.d0)
            qm(55) = qm(55) + m32*epi* (ordi**2+epi**2/12.d0)
            qm(56) = qm(56) + m33*epi* (ordi**2+epi**2/12.d0)
!
!         ON STOCKE LES VALEURS PRECEDENTES PAR COUCHE (NON CUMULEES)
!         (EPI*H)     CF HM
            zr(jobme+56+ (i-1)*nv+27) = q11*epi
            zr(jobme+56+ (i-1)*nv+28) = q12*epi
            zr(jobme+56+ (i-1)*nv+29) = q13*epi
            zr(jobme+56+ (i-1)*nv+30) = q22*epi
            zr(jobme+56+ (i-1)*nv+31) = q23*epi
            zr(jobme+56+ (i-1)*nv+32) = q33*epi
!         (EPI*ORDI*H)   CF HMF
            zr(jobme+56+ (i-1)*nv+33) = q11*epi*ordi
            zr(jobme+56+ (i-1)*nv+34) = q12*epi*ordi
            zr(jobme+56+ (i-1)*nv+35) = q13*epi*ordi
            zr(jobme+56+ (i-1)*nv+36) = q22*epi*ordi
            zr(jobme+56+ (i-1)*nv+37) = q23*epi*ordi
            zr(jobme+56+ (i-1)*nv+38) = q33*epi*ordi
!         (ZI+1**3-ZI**3)*H/3)    CF HF
            zr(jobme+56+ (i-1)*nv+39) = q11*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+40) = q12*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+41) = q13*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+42) = q22*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+43) = q23*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+44) = q33*epi* (ordi**2+epi**2/ 12.d0)
!         DILATATION THERMIQUE
            zr(jobme+56+ (i-1)*nv+45) = m11*epi
            zr(jobme+56+ (i-1)*nv+46) = m12*epi
            zr(jobme+56+ (i-1)*nv+47) = m13*epi
            zr(jobme+56+ (i-1)*nv+48) = m21*epi
            zr(jobme+56+ (i-1)*nv+49) = m22*epi
            zr(jobme+56+ (i-1)*nv+50) = m23*epi
            zr(jobme+56+ (i-1)*nv+51) = m31*epi
            zr(jobme+56+ (i-1)*nv+52) = m32*epi
            zr(jobme+56+ (i-1)*nv+53) = m33*epi
            zr(jobme+56+ (i-1)*nv+54) = m11*epi*ordi
            zr(jobme+56+ (i-1)*nv+55) = m12*epi*ordi
            zr(jobme+56+ (i-1)*nv+56) = m13*epi*ordi
            zr(jobme+56+ (i-1)*nv+57) = m21*epi*ordi
            zr(jobme+56+ (i-1)*nv+58) = m22*epi*ordi
            zr(jobme+56+ (i-1)*nv+59) = m23*epi*ordi
            zr(jobme+56+ (i-1)*nv+60) = m31*epi*ordi
            zr(jobme+56+ (i-1)*nv+61) = m32*epi*ordi
            zr(jobme+56+ (i-1)*nv+62) = m33*epi*ordi
            zr(jobme+56+ (i-1)*nv+63) = m11*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+64) = m12*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+65) = m13*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+66) = m21*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+67) = m22*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+68) = m23*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+69) = m31*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+70) = m32*epi* (ordi**2+epi**2/ 12.d0)
            zr(jobme+56+ (i-1)*nv+71) = m33*epi* (ordi**2+epi**2/ 12.d0)
!
!        STOCKAGE DES CRITERES
            zr(jobme+56 -1 + (i-1)*nv+79) = xt
            zr(jobme+56 -1 + (i-1)*nv+80) = xc
            zr(jobme+56 -1 + (i-1)*nv+81) = yt
            zr(jobme+56 -1 + (i-1)*nv+82) = yc
            zr(jobme+56 -1 + (i-1)*nv+83) = slt
 70     continue
        if (iret .eq. 0) then
        else if (iret.ne.nbcou) then
            call utmess('F', 'MODELISA5_63')
        endif
        qm(19) = eptot
        if (iret .eq. nbcou) then
            qm(20) = r8maem()
        else
            qm(20) = rhohom
        endif
!        -- INVERSION DE HF -- POUR CALCULER HC PAGE 242
        hf(1,1) = qm(13)
        hf(1,2) = qm(14)
        hf(1,3) = qm(15)
        hf(2,2) = qm(16)
        hf(2,3) = qm(17)
        hf(3,3) = qm(18)
        hf(2,1) = hf(1,2)
        hf(3,1) = hf(1,3)
        hf(3,2) = hf(2,3)
        do 90 k = 1, 3
            do 80 j = 1, 3
                hfinv(k,j) = 0.d0
 80         continue
 90     continue
        do 100 i = 1, 3
            hfinv(i,i) = 1.d0
100     continue
        call mgauss('NFVD', hf, hfinv, 3, 3,&
                    3, det, iret)
        do 120 k = 1, 2
            do 110 j = 1, 2
                da1i(k,j) = 0.d0
                c11(k,j) = 0.d0
                idi(k,j) = 0.d0
                ezd(k,j) = 0.d0
110         continue
120     continue
        hc11 = 0.d0
        hc22 = 0.d0
        hc12 = 0.d0
        do 220 icou = 1, nbcou
            epi = zr(jepor+3*icou-3)
            ordi = zr(jepor+3*icou-1)
!          QIJ : MATRICE DE COMPORTEMENT HI DANS LE REPERE UTILISATEUR
            hi(1,1) = zr(jobme+56+ (icou-1)*nv+3)
            hi(1,2) = zr(jobme+56+ (icou-1)*nv+4)
            hi(1,3) = zr(jobme+56+ (icou-1)*nv+5)
            hi(2,2) = zr(jobme+56+ (icou-1)*nv+6)
            hi(2,3) = zr(jobme+56+ (icou-1)*nv+7)
            hi(3,3) = zr(jobme+56+ (icou-1)*nv+8)
            hi(2,1) = hi(1,2)
            hi(3,1) = hi(1,3)
            hi(3,2) = hi(2,3)
            do 140 k = 1, 3
                do 130 j = 1, 3
                    ai(k,j) = 0.d0
130             continue
140         continue
            do 170 i = 1, 3
                do 160 j = 1, 3
                    do 150 k = 1, 3
                        ai(i,j) = ai(i,j) + hi(i,k)*hfinv(k,j)
150                 continue
160             continue
170         continue
!         TERMES DE LA MATRICE INTERVENANT DANS D1(Z)
!      CF. DHAT-BATOZ VOL 2 PAGE 243
!      DAI1 MATRICE (2,2) CONSTANTE PAR COUCHE
!      TERME 1,1 : A11+A33 TERME 1,2 : A13+A32
!      TERME 2,1 : A31+A23 TERME 2,2 : A22+A33
            da1i(1,1) = ai(1,1) + ai(3,3)
            da1i(1,2) = ai(1,3) + ai(3,2)
            da1i(2,1) = ai(3,1) + ai(2,3)
            da1i(2,2) = ai(2,2) + ai(3,3)
!
            zr(jobme+56+ (icou-1)*nv+15) = ai(1,1)
            zr(jobme+56+ (icou-1)*nv+16) = ai(2,1)
            zr(jobme+56+ (icou-1)*nv+17) = ai(3,1)
            zr(jobme+56+ (icou-1)*nv+18) = ai(1,2)
            zr(jobme+56+ (icou-1)*nv+19) = ai(2,2)
            zr(jobme+56+ (icou-1)*nv+20) = ai(3,2)
            zr(jobme+56+ (icou-1)*nv+21) = ai(1,3)
            zr(jobme+56+ (icou-1)*nv+22) = ai(2,3)
            zr(jobme+56+ (icou-1)*nv+23) = ai(3,3)
!
!          TERMES CONSTANT DANS D1(Z)
            do 190 k = 1, 2
                do 180 j = 1, 2
                    idi(k,j) = ezd(k,j) - ((ordi-epi/2.d0)**2)*da1i(k, j)/2.d0
                    ezd(k,j) = ezd(k,j) + epi*ordi*da1i(k,j)
180             continue
190         continue
!          MATRICE DE COMPORTEMENT HTAU DANS REPERE UTILISATEUR
            hti(1,1) = zr(jobme+56+ (icou-1)*nv+12)
            hti(2,2) = zr(jobme+56+ (icou-1)*nv+13)
            hti(1,2) = zr(jobme+56+ (icou-1)*nv+14)
            hti(2,1) = hti(1,2)
!           -- INVERSION DE HTI ---
            det = hti(1,1)*hti(2,2) - hti(1,2)*hti(2,1)
            if (abs(det) .lt. epsi) then
                c11i(1,1) = 0.d0
                c11i(1,2) = 0.d0
                c11i(2,1) = 0.d0
                c11i(2,2) = 0.d0
                htiinv(1,1) = 0.d0
                htiinv(2,2) = 0.d0
                htiinv(1,2) = 0.d0
                htiinv(2,1) = 0.d0
            else
                htiinv(1,1) = hti(2,2)/det
                htiinv(2,2) = hti(1,1)/det
                htiinv(1,2) = -hti(1,2)/det
                htiinv(2,1) = -hti(2,1)/det
            endif
!         CALCUL DE C11
            call utbtab('ZERO', 2, 2, htiinv, idi,&
                        xab, c11i1)
            call utctab('ZERO', 2, 2, 2, htiinv,&
                        da1i, idi, xab, c11i2)
            call utctab('ZERO', 2, 2, 2, htiinv,&
                        idi, da1i, xab, c11i3)
            call utbtab('ZERO', 2, 2, htiinv, da1i,&
                        xab, c11i4)
            x1 = epi/4.d0
            x2 = (3.d0*ordi*ordi*epi+epi**3/4.d0)/24.d0
            x3 = x2
            x4 = (5.d0*ordi**4*epi+2.5d0*ordi*ordi*epi**3+epi**5/ 16.d0)/ 80.d0
            do 210 k = 1, 2
                do 200 j = 1, 2
                    c11i(k,j) = x1*c11i1(k,j) + x2*c11i2(k,j) + x3*c11i3(k,j) + x4*c11i4(k,j)
                    c11(k,j) = c11(k,j) + c11i(k,j)
200             continue
210         continue
            hc11 = hc11 + epi*hti(1,1)
            hc22 = hc22 + epi*hti(2,2)
            hc12 = hc12 + epi*hti(1,2)
!        -- INVERSION DE C11 LOCALE --- HC = C11INV LOCALE
            det = c11i(1,1)*c11i(2,2) - c11i(1,2)*c11i(2,1)
            if (abs(det) .lt. epsi) then
                c11inv(1,1) = 0.d0
                c11inv(2,2) = 0.d0
                c11inv(1,2) = 0.d0
                c11inv(2,1) = 0.d0
            else
                c11inv(1,1) = c11i(2,2)/det
                c11inv(2,2) = c11i(1,1)/det
                c11inv(1,2) = -c11i(1,2)/det
                c11inv(2,1) = -c11i(2,1)/det
            endif
            zr(jobme+56+ (icou-1)*nv+72) = c11i(1,1)
            zr(jobme+56+ (icou-1)*nv+73) = c11i(2,2)
            zr(jobme+56+ (icou-1)*nv+74) = c11i(1,2)
            zr(jobme+56+ (icou-1)*nv+75) = c11inv(1,1)
            zr(jobme+56+ (icou-1)*nv+76) = c11inv(2,2)
            zr(jobme+56+ (icou-1)*nv+77) = c11inv(1,2)
220     continue
! FIN BOUCLE SUR LES COUCHES
!        -- INVERSION DE C11 INTEGREE SUR L'EPAISSEUR
!        -- INVERSION DE C11 GLOBALE --- HC = C11INV GLOBALE
        det = c11(1,1)*c11(2,2) - c11(1,2)*c11(2,1)
        if (abs(det) .lt. epsi) then
            c11inv(1,1) = 0.d0
            c11inv(2,2) = 0.d0
            c11inv(1,2) = 0.d0
            c11inv(2,1) = 0.d0
        else
            c11inv(1,1) = c11(2,2)/det
            c11inv(2,2) = c11(1,1)/det
            c11inv(1,2) = -c11(1,2)/det
            c11inv(2,1) = -c11(2,1)/det
        endif
!       COEFFICENTS DE CISAILLEMENT MOYENS
        if (abs(hc11) .lt. epsi) then
            k11 = 0.d0
        else
            k11 = c11inv(1,1)/hc11
        endif
        if (abs(hc22) .lt. epsi) then
            k22 = 0.d0
        else
            k22 = c11inv(2,2)/hc22
        endif
        if (abs(hc12) .lt. epsi) then
            k12 = 0.d0
        else
            k12 = c11inv(1,2)/hc12
        endif
        qm(21) = c11(1,1)
        qm(22) = c11(2,2)
        qm(23) = c11(1,2)
        qm(24) = c11inv(1,1)
        qm(25) = c11inv(2,2)
        qm(26) = c11inv(1,2)
        qm(27) = k11
        qm(28) = k22
        qm(29) = k12
        if (nimpr .gt. 0) then
            write (ifr,1000)
            write (ifr,*) 'COEFFICIENTS HOMOGENEISES MECANIQUES :'
            write (ifr,*) 'BLOCS R,Q,P :'
            write (ifr,*) '   QM(1-18)=   :'
            write (ifr,'(5(2X,E13.6))') (qm(i),i=1,18)
            write (ifr,*) 'EPAISSEUR - MASSE VOL. :'
!
            write (ifr,'(5(2X,E13.6))') qm(19),qm(20)
            write (ifr,*) 'CISAILLEMENT - DCI11, DCI22, DCI12 :'
!
            write (ifr,'(5(2X,E13.6))') qm(21),qm(22),qm(23)
            write (ifr,*) 'CISAILLEMENT - DC11, DC22, DC12 :'
!
            write (ifr,'(5(2X,E13.6))') qm(24),qm(25),qm(26)
            write (ifr,*) 'CISAILLEMENT - K11, K22, K12 :'
!
            write (ifr,'(5(2X,E13.6))') qm(27),qm(28),qm(29)
            write (ifr,1000)
        endif
        do 230 i = 1, 56
            zr(jobme-1+i) = qm(i)
            zc(jobmc-1+i) = 0.d0
230     continue
        do 240 i = 57, 56 + nv*nbcou
            zc(jobmc-1+i) = 0.d0
240     continue
    endif
!
    if (ther) then
!
!     ------ CARACTERISTIQUES THERMIQUES ------
!
        call wkvect('&&OP0056.EPOR', 'V V R', 3*nbcou, jepor)
        call jeexin(multic//'.MATERIAU.NOMRC ', nobj)
        if (nobj .eq. 0) then
            call wkvect(multic//'.MATERIAU.NOMRC ', 'G V K32', nbcou+2, jrela)
        endif
        zk32(jrela) = 'THER_COQMU      '
        lonobj = 31 + 3*nbcou
        call codent(1, 'D0', k6)
        call wkvect(multic//'.CPT.'//k6//'.VALK', 'G V K16', 2*lonobj, jmate)
        call jeecra(multic//'.CPT.'//k6//'.VALK', 'LONUTI', lonobj)
        call wkvect(multic//'.CPT.'//k6//'.VALR', 'G V R', lonobj, jobth)
        call jeecra(multic//'.CPT.'//k6//'.VALR', 'LONUTI', lonobj)
        call wkvect(multic//'.CPT.'//k6//'.VALC', 'G V C', lonobj, jobtc)
        call jeecra(multic//'.CPT.'//k6//'.VALC', 'LONUTI', 0)
        eptot = 0.d0
        do 250 i = 1, 31
            call codent(i, 'G', num)
            zk16(jmate+i-1) = 'HOM_'//num
250     continue
        do 270 icou = 1, nbcou
            call getvr8('COUCHE', 'EPAIS', iocc=icou, scal=epais, nbret=n)
            call getvid('COUCHE', 'MATER', iocc=icou, scal=mater, nbret=n)
            call getvr8('COUCHE', 'ORIENTATION', iocc=icou, scal=orien, nbret=n)
            zk32(jrela+1+icou) = mater
            call codent(icou, 'G', num)
            do 260 i = 1, 3
                call codent(i, 'G', val)
                zk16(jmate+31+3* (icou-1)+i-1) = 'C'//num//'_V'//val
260         continue
            zr(jepor-1+3*icou-2) = epais
            zr(jepor-1+3*icou-1) = orien
            eptot = eptot + epais
270     continue
!
        epais = -0.5d0*eptot
        h = 0.5d0*eptot
        h2 = h*h
        h3 = h*h2
        h4 = h*h3
        do 280 i = 1, 31
            qt(i) = 0.d0
280     continue
        do 290 i = 1, nbcou
            nbres = 4
            nomres(1) = 'LAMBDA_L'
            nomres(2) = 'LAMBDA_T'
            nomres(3) = 'LAMBDA_N'
            nomres(4) = 'RHO_CP'
            k8b = ' '
            call rcvale(zk32(jrela+i+1) (1:8), 'THER_ORTH', 0, k8b, [r8bid],&
                        nbres, nomres, valres, icodre, 0)
            laml = valres(1)
            lamt = valres(2)
            lamn = valres(3)
            cp = valres(4)
            c = cos(zr(jepor+3*i-2)*r8dgrd())
            s = sin(zr(jepor+3*i-2)*r8dgrd())
            c2 = c*c
            s2 = s*s
            epais = epais + zr(jepor+3*i-3)
            zr(jepor+3*i-1) = epais - 0.5d0*zr(jepor+3*i-3)
            epi = zr(jepor+3*i-3)
            ordi = zr(jepor+3*i-1)
            zr(jobth+31+3* (i-1)) = epi
            zr(jobth+31+3* (i-1)+1) = zr(jepor+3*i-2)
            zr(jobth+31+3* (i-1)+2) = ordi
            rzkp = ordi + epi/2
            rzkm = ordi - epi/2
            dif3 = (rzkp**3-rzkm**3)/3.d0
            dif4 = (rzkp**4-rzkm**4)/4.d0
            dif5 = (rzkp**5-rzkm**5)/5.d0
            a11 = epi + dif5/h4 - 2.d0*dif3/h2
            a12 = -epi/ (2.d0*h) + dif3/ (2.d0*h2) + dif4/ (2.d0*h3) - dif5/ (2.d0*h4)
            a13 = epi*ordi/ (2.d0*h) + dif3/ (2.d0*h2) - dif4/ (2.d0* h3) - dif5/ (2.d0*h4)
            a22 = dif3/ (4.d0*h2) + dif5/ (4.d0*h4) - dif4/ (2.d0*h3)
            a23 = -dif3/ (4.d0*h2) + dif5/ (4.d0*h4)
            a33 = dif3/ (4.d0*h2) + dif5/ (4.d0*h4) + dif4/ (2.d0*h3)
            b11 = 4.d0*dif3/h4
            b12 = epi*ordi/h3 - 2.d0*dif3/h4
            b13 = -epi*ordi/h3 - 2.d0*dif3/h4
            b22 = epi/ (4.d0*h2) + dif3/h4 - epi*ordi/h3
            b23 = dif3/h4 - epi/ (4.d0*h2)
            b33 = epi/ (4.d0*h2) + dif3/h4 + epi*ordi/h3
            l11 = c2*laml + s2*lamt
            l22 = s2*laml + c2*lamt
            l12 = c*s* (laml-lamt)
            qt(1) = qt(1) + a11*l11
            qt(2) = qt(2) + a11*l22
            qt(3) = qt(3) + a11*l12
            qt(4) = qt(4) + a12*l11
            qt(5) = qt(5) + a12*l22
            qt(6) = qt(6) + a12*l12
            qt(7) = qt(7) + a13*l11
            qt(8) = qt(8) + a13*l22
            qt(9) = qt(9) + a13*l12
            qt(10) = qt(10) + a22*l11
            qt(11) = qt(11) + a22*l22
            qt(12) = qt(12) + a22*l12
            qt(13) = qt(13) + a23*l11
            qt(14) = qt(14) + a23*l22
            qt(15) = qt(15) + a23*l12
            qt(16) = qt(16) + a33*l11
            qt(17) = qt(17) + a33*l22
            qt(18) = qt(18) + a33*l12
            qt(19) = qt(19) + b11*lamn
            qt(20) = qt(20) + b12*lamn
            qt(21) = qt(21) + b13*lamn
            qt(22) = qt(22) + b22*lamn
            qt(23) = qt(23) + b23*lamn
            qt(24) = qt(24) + b33*lamn
            qt(25) = qt(25) + a11*cp
            qt(26) = qt(26) + a12*cp
            qt(27) = qt(27) + a13*cp
            qt(28) = qt(28) + a22*cp
            qt(29) = qt(29) + a23*cp
            qt(30) = qt(30) + a33*cp
290     continue
        qt(31) = eptot
        if (nimpr .gt. 0) then
            write (ifr,1000)
            write (ifr,*) 'COEFFICIENTS HOMOGENEISES THERMIQUES :'
            write (ifr,*) 'BLOCS A,B,C - EPAISSEUR :'
            write (ifr,*) '   QT(1-31)=   :'
            write (ifr,'(5(2X,E13.6))') qt
            write (ifr,1000)
        endif
        do 300 i = 1, 31
            zr(jobth-1+i) = qt(i)
            zc(jobtc-1+i) = dcmplx(0.d0,0.d0)
300     continue
    endif
!
    1000 format (/,80 ('-'))
!
    call jedema()
end subroutine
