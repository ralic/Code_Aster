subroutine pecapo(resu, modele, cara, nh)
    implicit   none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecham.h"
#include "asterfort/pecap1.h"
#include "asterfort/pecap2.h"
#include "asterfort/pecap3.h"
#include "asterc/r8dgrd.h"
#include "asterfort/rcvale.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcopi.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/tbnuli.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: nh
    character(len=*) :: resu, modele, cara
!     ------------------------------------------------------------------
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "CARA_POUTRE"
!     ------------------------------------------------------------------
!
    integer :: nbtors, nbgauc, nbcisa, iret, nt, ibid, nopt, ntab, nct, ilign, ncty, nctz, ngm
    integer :: ngi, ngri, idgrmi, nrt, nbrt
    parameter    ( nbtors = 1 , nbgauc = 1 , nbcisa = 8 , nbrt = 1 )
    real(kind=8) :: valpar(nbcisa), ay, az, ey, ez, pcty, pctz, r8b, rt, jx, s, yg, zg, iy, iz
    real(kind=8) :: alpha, iomega
    character(len=8) :: k8b, temper, tempe1, tempe2, ptors(nbtors), pgauc(nbgauc), pcisa(nbcisa)
    character(len=8) :: prt(nbrt)
    character(len=16) :: option
    character(len=19) :: nomtab
    character(len=24) :: chgeom, chcara(18), chharm, nogrma, noma, nomail
    complex(kind=8) :: c16b
    real(kind=8) :: k1, k2, ky, kz, kyeq, kzeq, iyeq, izeq, seq, ee, gg, hh, ksi, nu
    real(kind=8) :: c1, c2, phi1, phi2, alphar, cos2, sin2, alpheq, ygeq, zgeq
    character(len=16) :: ll
    character(len=8) :: mater
    integer :: icodre
    integer :: ilignm, n1
    integer :: iarg
!     ------------------------------------------------------------------
    data  ptors / 'JX'      /
    data  prt   / 'RT'      /
    data  pgauc / 'JG'      /
    data  pcisa / 'AY'      ,  'AZ'      ,  'EY'      ,  'EZ'      ,&
     &              'PCTY'    ,  'PCTZ'    ,  'KY'      ,  'KZ'      /
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE LA TABLE A COMPLETER ISSUE DE L'OPTION
! --- CARA_GEOM DE POST_ELEM :
!     ----------------------
    call getvid('CARA_POUTRE', 'CARA_GEOM', 1, iarg, 0,&
                k8b, ntab)
    if (ntab .ne. 0) then
        call getvid('CARA_POUTRE', 'CARA_GEOM', 1, iarg, 1,&
                    nomtab, ntab)
        call tbcopi('G', nomtab, resu)
    else
        call u2mess('F', 'UTILITAI3_59')
    endif
!
    option = 'MASS_INER'
!
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
!
! --- RECUPERATION DU MAILLAGE INITIAL :
!     --------------------------------
    call tbexp2(resu, 'MAILLAGE')
    call tbliva(resu, 0, k8b, ibid, r8b,&
                c16b, 'TOUT', k8b, r8b, 'MAILLAGE',&
                k8b, ibid, r8b, c16b, noma,&
                iret)
    nomail=noma
!
    ngm = 0
    nt = 0
!
!     INSERTION DU PARAMETRE 'RT' DANS LA TABLE 'RESU'
    call tbajpa(resu, 1, prt, 'R')
    call getvtx('CARA_POUTRE', 'TOUT', 1, iarg, 0,&
                k8b, nt)
    if (nt .eq. 0) then
        call getvtx('CARA_POUTRE', 'GROUP_MA', 1, iarg, 0,&
                    k8b, ngm)
        if (ngm .ne. 0) then
            ngm = 1
            call getvtx('CARA_POUTRE', 'GROUP_MA', 1, iarg, ngm,&
                        nogrma, ngm)
            noma=nogrma
            call getvr8('CARA_POUTRE', 'LONGUEUR', 1, iarg, 1,&
                        hh, n1)
            call getvtx('CARA_POUTRE', 'LIAISON', 1, iarg, 1,&
                        ll, n1)
            call getvid('CARA_POUTRE', 'MATERIAU', 1, iarg, 1,&
                        mater, n1)
            if (n1 .eq. 0) then
                nu=0.d0
            else
                k8b = ' '
                call rcvale(mater, 'ELAS', 0, k8b, r8b,&
                            1, 'NU      ', nu, icodre, 1)
            endif
        endif
    endif
!
! ---   RECUPERATION DU NUMERO DE LIGNE DE LA TABLE RESULTAT POUR LA
! ---   VARIABLE "NOMA" :
!       ---------------
    call tbexp2(resu, 'LIEU')
    call tbnuli(resu, 1, 'LIEU', ibid, r8b,&
                c16b, nomail, r8b, k8b, ilignm)
    call tbnuli(resu, 1, 'LIEU', ibid, r8b,&
                c16b, noma, r8b, k8b, ilign)
    if (ilign .lt. 0) ilign = 0
!
! ---   RECUPERATION DE L'OPTION DE CALCUL RELATIVE AUX
! ---   CARACTERISTIQUES DE POUTRE :
!       --------------------------
    call getvtx('CARA_POUTRE', 'OPTION', 1, iarg, 0,&
                k8b, nopt)
    if (nopt .eq. 0) then
        call u2mess('F', 'UTILITAI3_60')
    endif
!
    call getvtx('CARA_POUTRE', 'OPTION', 1, iarg, 1,&
                option, nopt)
!
! ---   LES SEULES OPTIONS PERMISES, POUR LE MOMENT, SONT
! ---   'CARA_TORSION' ET 'CARA_CISAILLEMENT':
!       ------------------------------------
    if (option .ne. 'CARA_TORSION' .and. option .ne. 'CARA_CISAILLEMEN' .and. option .ne.&
        'CARA_GAUCHI') then
        call u2mesk('F', 'UTILITAI3_61', 1, option)
    endif
!
!     -----------------------------------------------------------
! --- -CALCUL DE LA CONSTANTE DE TORSION
! --- -AJOUT DU RAYON DE TORSION DANS LA TABLE 'RESU'
!     -----------------------------------------------------------
!
! --- RECUPERATION DU RAYON DE TORSION :
!     --------------------------------
    if (option .eq. 'CARA_TORSION') then
        call getvr8('CARA_POUTRE', 'RT', 1, iarg, 0,&
                    rt, nrt)
        if (nrt .ne. 0) then
            nrt=-nrt
            call getvr8('CARA_POUTRE', 'RT', 1, iarg, 1,&
                        rt, nrt)
        endif
!
! --- RECUPERATION DU RESULTAT DE TYPE EVOL_THER DONT L'INTEGRALE
! --- SUR LA SECTION DE LA POUTRE VA DONNER LA CONSTANTE DE TORSION :
!     -------------------------------------------------------------
        call getvid('CARA_POUTRE', 'LAPL_PHI', 1, iarg, 0,&
                    k8b, nct)
        if (nct .ne. 0) then
            nct=-nct
            call getvid('CARA_POUTRE', 'LAPL_PHI', 1, iarg, 1,&
                        temper, nct)
        else
            call u2mess('F', 'UTILITAI3_62')
        endif
!
! --- RECUPERATION DES MAILLES DE BORD CONSTITUANT LES
! --- CONTOURS INTERIEURS :
!     -------------------
        call getvtx('CARA_POUTRE', 'GROUP_MA_INTE', 1, iarg, 0,&
                    k8b, ngi)
        if (ngi .ne. 0) then
            ngi = -ngi
            call wkvect('&&PECAPO.GRMA_INTE', 'V V K24', ngi, idgrmi)
            call getvtx('CARA_POUTRE', 'GROUP_MA_INTE', 1, iarg, ngi,&
                        zk24(idgrmi), ngri)
        else
            call wkvect('&&PECAPO.GRMA_INTE', 'V V K24', 1, idgrmi)
        endif
!
! --- CALCUL DE LA CONSTANTE DE TORSION JX :
!     ------------------------------------
        call pecap1(chgeom, temper, ngi, zk24(idgrmi), jx)
!
! --- AJOUT DE JX ET RT DANS LA TABLE 'RESU' :
!     --------------------------------------
        if (nrt .ne. 0) then
            call tbajli(resu, nbrt, prt, ibid, rt,&
                        c16b, k8b, ilign)
        endif
        call tbajli(resu, nbtors, ptors, ibid, jx,&
                    c16b, k8b, ilign)
!         ILIGN = 1
!
!     ------------------------------------------
! --- -CALCUL DES COEFFICIENTS DE CISAILLEMENT -
! --- -ET DES COORDONNEES DU CENTRE DE TORSION -
!     ------------------------------------------
    else if (option.eq.'CARA_CISAILLEMEN') then
!
! --- RECUPERATION DE 2 RESULTATS DE TYPE EVOL_THER DESTINES A
! --- CALCULER LES COEFFICIENTS DE CISAILLEMENT ET LES COORDONNEES
! --- DU CENTRE DE CISAILLEMENT/TORSION :
!     ---------------------------------
        call getvid('CARA_POUTRE', 'LAPL_PHI_Y', 1, iarg, 0,&
                    k8b, ncty)
        if (ncty .ne. 0) then
            call getvid('CARA_POUTRE', 'LAPL_PHI_Y', 1, iarg, 1,&
                        tempe1, ncty)
        else
            call u2mess('F', 'UTILITAI3_63')
        endif
!
        call getvid('CARA_POUTRE', 'LAPL_PHI_Z', 1, iarg, 0,&
                    k8b, nctz)
        if (nctz .ne. 0) then
            call getvid('CARA_POUTRE', 'LAPL_PHI_Z', 1, iarg, 1,&
                        tempe2, nctz)
        else
            call u2mess('F', 'UTILITAI3_64')
        endif
!
! --- RECUPERATION DANS LA TABLE DE LA SURFACE DE LA SECTION S,
! --- DES INERTIES PRINCIPALES IY ET IZ, DE L'ANGLE ALPHA FORME
! --- PAR LES AXES PRINCIPAUX D'INERTIE AVEC LES AXES GLOBAUX ET
! --- DES COORDONNEES DU CENTRE DE GRAVITE DANS LE REPERE GLOBAL :
!     ----------------------------------------------------------
        call tbexp2(resu, 'LIEU')
        call tbexp2(resu, 'A')
        call tbexp2(resu, 'IY')
        call tbexp2(resu, 'IZ')
        call tbexp2(resu, 'ALPHA')
        call tbexp2(resu, 'CDG_Y')
        call tbexp2(resu, 'CDG_Z')
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'A',&
                    k8b, ibid, s, c16b, k8b,&
                    iret)
        if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'IY',&
                    k8b, ibid, iy, c16b, k8b,&
                    iret)
        if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'IZ',&
                    k8b, ibid, iz, c16b, k8b,&
                    iret)
        if (iret .ne. 0) call u2mess('F', 'ALGELINE_7')
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'ALPHA',&
                    k8b, ibid, alpha, c16b, k8b,&
                    iret)
        call assert(iret .eq. 0)
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'CDG_Y',&
                    k8b, ibid, yg, c16b, k8b,&
                    iret)
        call assert(iret.eq.0)
        call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                    c16b, noma, k8b, r8b, 'CDG_Z',&
                    k8b, ibid, zg, c16b, k8b,&
                    iret)
        call assert(iret.eq.0)
!
! --- CALCUL DES COORDONNEES DU CENTRE DE CISAILLEMENT/TORSION EY ET EZ
! --- ET DES COEFFICIENTS DE CISAILLEMENT
! --- (OU PLUTOT DE LEUR INVERSE) AY ET AZ :
!     ------------------------------------
        call pecap2(chgeom, iy, iz, s, alpha,&
                    yg, zg, tempe1, tempe2, ay,&
                    az, ey, ez, pcty, pctz)
!
!     ON CHANGE DE SIGNE EY EZ CAR ON ATTEND CG ET NON PAS GC
!     CF DOC MACRO_CARA_POUTRE
        valpar(1) = ay
        valpar(2) = az
        valpar(3) = -ey
        valpar(4) = -ez
        valpar(5) = -pcty
        valpar(6) = -pctz
        valpar(7) = 0.d0
        valpar(8) = 0.d0
        call tbajli(resu, nbcisa, pcisa, ibid, valpar,&
                    c16b, k8b, ilign)
        if (nomail .ne. noma) then
            call tbexp2(resu, 'KY')
            call tbexp2(resu, 'KZ')
!       CAS OU IL FAUT FAIRE UN CUMUL DANS LE MAILLAGE COMPLET
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'A',&
                        k8b, ibid, seq, c16b, k8b,&
                        iret)
            call assert(iret.eq.0)
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'IY',&
                        k8b, ibid, iyeq, c16b, k8b,&
                        iret)
            call assert(iret.eq.0)
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'IZ',&
                        k8b, ibid, izeq, c16b, k8b,&
                        iret)
            call assert(iret.eq.0)
!
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'KY',&
                        k8b, ibid, ky, c16b, k8b,&
                        iret)
            if (iret .ne. 0) ky=0.d0
!
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'KZ',&
                        k8b, ibid, kz, c16b, k8b,&
                        iret)
            if (iret .ne. 0) kz=0.d0
!
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'ALPHA',&
                        k8b, ibid, alpheq, c16b, k8b,&
                        iret)
            call assert(iret .eq. 0)
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'CDG_Y',&
                        k8b, ibid, ygeq, c16b, k8b,&
                        iret)
            call assert(iret .eq. 0)
            call tbliva(resu, 1, 'LIEU', ibid, r8b,&
                        c16b, nomail, k8b, r8b, 'CDG_Z',&
                        k8b, ibid, zgeq, c16b, k8b,&
                        iret)
            call assert(iret .eq. 0)
!
!         VECTEUR GEQ-GI DANS LE REPERE GLOBAL
!
!         DYG=YG-YGEQ
!         DZG=ZG-ZGEQ
!
!         VECTEUR GEQ-GI DANS LE REPERE PRINCIPAL DE NOMA
!
!         ALPHAI=ALPHA*R8DGRD()
!         ZGI= COS(ALPHAI)*DYG+SIN(ALPHAI)*DZG
!         ZGI=-SIN(ALPHAI)*DYG+COS(ALPHAI)*DZG
!
!         MOMENTS D'INERTIE PAR RAPPORT A GEQ,YI,ZI
!         TRANSPORT SUPPRIME CAR NON JUSTIFIE
!         DONNE DES RESULTATS FAUX SUR ZZZZ105H
!          IY = IY + S*ZGI**2
!          IZ = IZ + S*ZGI**2
!
!         SEUL LE RAPPORT E/G EST IMPORTANT
!
            gg=1.d0/2.d0/(1.d0+nu)
            ee=1.d0
            ksi=1.d0
            if (ll .eq. 'ROTULE') ksi=4.d0
            c1 = 12.d0*ee*iz
            c2 = 12.d0*ee*iy
            phi1=c1/( (s/ay)*gg*(hh**2) )
            phi2=c2/( (s/az)*gg*(hh**2) )
            k1=c1/(hh**3*(ksi+phi1))
            k2=c2/(hh**3*(ksi+phi2))
!
            alphar = (alpha-alpheq)*r8dgrd()
            cos2 = cos(alphar)**2
            sin2 = sin(alphar)**2
            ky=ky+ ( k1 * cos2 + k2 * sin2)
            kz=kz+ ( k1 * sin2 + k2 * cos2)
!
            kyeq=(12.d0*ee*izeq)/ (gg*seq*hh**2)/( 12.d0*ee*izeq/ky/&
            hh**3 -1.d0 )
!
            kzeq=(12.d0*ee*iyeq)/ (gg*seq*hh**2)/( 12.d0*ee*iyeq/kz/&
            hh**3 -1.d0 )
!
!         NOUVEAUX AY ET AZ POUR LE MAILLAGE
            valpar(1) = 1.d0/kyeq
            valpar(2) = 1.d0/kzeq
            call tbajli(resu, 2, pcisa(1), ibid, valpar(1),&
                        c16b, k8b, ilignm)
            valpar(7) = ky
            valpar(8) = kz
            call tbajli(resu, 2, pcisa(7), ibid, valpar(7),&
                        c16b, k8b, ilignm)
        endif
!
!     ------------------------------------------
! --- -CALCUL DE LA CONSTANTE DE GAUCHISSEMENT -
!     ------------------------------------------
    else if (option.eq.'CARA_GAUCHI') then
!
! --- RECUPERATION DU RESULTAT DE TYPE EVOL_THER DONT L'INTEGRALE
! --- SUR LA SECTION DE LA POUTRE VA DONNER LA CONSTANTE DE
! --- GAUCHISSEMENT :
!     -------------
        call getvid('CARA_POUTRE', 'LAPL_PHI', 1, iarg, 0,&
                    k8b, nct)
        if (nct .ne. 0) then
            call getvid('CARA_POUTRE', 'LAPL_PHI', 1, iarg, 1,&
                        temper, nct)
        else
            call u2mess('F', 'UTILITAI3_62')
        endif
!
! --- CALCUL DE LA CONSTANTE DE GAUCHISSEMENT IOMEGA :
!     ----------------------------------------------
        call pecap3(chgeom, temper, iomega)
!
        call tbajli(resu, nbgauc, pgauc, ibid, iomega,&
                    c16b, k8b, ilign)
    endif
!
! --- MENAGE
    call jedetr('&&PECAPO.GRMA_INTE')
!
    call jedema()
end subroutine
