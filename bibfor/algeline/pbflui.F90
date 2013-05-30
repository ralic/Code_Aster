subroutine pbflui(umoy, hmoy, rmoy, long, cf0,&
                  mcf0, fsvr, icoq, imod, nbm,&
                  rki, tcoef, s1, s2, ysol)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! RESOLUTION DU PROBLEME FLUIDE INSTATIONNAIRE : ROUTINE CHAPEAU
! NOTA BENE :
! LE PROBLEME FLUIDE INSTATIONNAIRE EST RESOLU POUR UNE VITESSE DE
! L'ECOULEMENT MOYEN EGALE A UMOY, EN CONSIDERANT UN MOUVEMENT DE LA
! COQUE ICOQ SUIVANT LE MODE IMOD A LA FREQUENCE COMPLEXE S=S1+J*S2
! APPELANT : BIJMOC, BMOCCA
!-----------------------------------------------------------------------
!  IN : UMOY   : VITESSE DE L'ECOULEMENT MOYEN
!  IN : HMOY   : JEU ANNULAIRE MOYEN
!  IN : RMOY   : RAYON MOYEN
!  IN : LONG   : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
!  IN : CF0    : COEFFICIENT DE FROTTEMENT VISQUEUX
!  IN : MCF0   : EXPOSANT VIS-A-VIS DU NOMBRE DE REYNOLDS
!  IN : FSVR   : OBJET .FSVR DU CONCEPT TYPE_FLUI_STRU
!  IN : ICOQ   : INDICE CARACTERISANT LA COQUE SUR LAQUELLE ON TRAVAILLE
!                ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE
!  IN : IMOD   : INDICE DU MODE CONSIDERE
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : RKI    : ORDRE DE COQUE DU MODE CONSIDERE
!  IN : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
!  IN : S1     : PARTIE REELLE     DE LA FREQUENCE COMPLEXE
!  IN : S2     : PARTIE IMAGINAIRE DE LA FREQUENCE COMPLEXE
! OUT : YSOL   : TABLEAU SOLUTION (VECTEUR T(UI*,VI*,PI*) TABULE EN Z)
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/pbflga.h'
    include 'asterfort/pbflkc.h'
    include 'asterfort/pbflso.h'
    include 'asterfort/pbflu0.h'
    include 'asterfort/pbflvp.h'
    include 'asterfort/profpr.h'
    include 'asterfort/wkvect.h'
    real(kind=8) :: umoy, hmoy, rmoy, long, cf0, mcf0, fsvr(7)
    integer :: icoq, imod, nbm
    real(kind=8) :: rki, tcoef(10, nbm), s1, s2
    complex(kind=8) :: ysol(3, 101)
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: icond, id, igama, iharm, ikcal, iki, ilbda
    integer :: ipass
    real(kind=8) :: coepr1, coepr2, r1, r2, rhof, rkip, wpr
!
!-----------------------------------------------------------------------
    call jemarq()
!
! --- 1.INITIALISATIONS ET CREATION DE VECTEURS DE TRAVAIL
!
    rhof = fsvr(1)
!
    r1 = rmoy - hmoy/2.d0
    r2 = rmoy + hmoy/2.d0
!
    call wkvect('&&PBFLUI.TEMP.LBDA', 'V V C', 3, ilbda)
    call wkvect('&&PBFLUI.TEMP.KCAL', 'V V C', 3*4, ikcal)
    call wkvect('&&PBFLUI.TEMP.COND', 'V V R', 3, icond)
    call wkvect('&&PBFLUI.TEMP.GAMA', 'V V C', 3, igama)
    call wkvect('&&PBFLUI.TEMP.PASS', 'V V C', 3*3, ipass)
    call wkvect('&&PBFLUI.TEMP.D', 'V V R', 6, id)
!
    call wkvect('&&PBFLUI.TEMP.KI', 'V V C', 4*3, iki)
    call wkvect('&&PBFLUI.TEMP.HARM', 'V V R', 6, iharm)
!
! --- 2.RESOLUTION
!
    call profpr(icoq, rki, r1, r2, coepr1,&
                coepr2, wpr)
    rkip = rki/dble(sqrt(wpr))
!
    if (umoy .lt. 1.d-5) then
!
        call pbflu0(rhof, hmoy, rmoy, long, icoq,&
                    imod, nbm, rkip, tcoef, zr( id))
!
    else
!
        call pbflvp(umoy, hmoy, rmoy, cf0, mcf0,&
                    rkip, s1, s2, zc(ilbda))
!
        call pbflkc(umoy, rhof, hmoy, rmoy, long,&
                    cf0, mcf0, icoq, imod, nbm,&
                    rkip, tcoef, s1, s2, zc(iki),&
                    zc(ilbda), zc(ikcal), zc(ipass))
!
        call pbflga(umoy, hmoy, rmoy, long, cf0,&
                    fsvr, icoq, imod, nbm, tcoef,&
                    s1, s2, zc(ilbda), zc(ikcal), zr(icond),&
                    zc(igama))
!
    endif
!
    call pbflso(umoy, rmoy, long, icoq, imod,&
                nbm, rkip, tcoef, zr(iharm), zc(ilbda),&
                zc(ikcal), zc(ipass), zr(icond), zc(igama), zr(id),&
                ysol)
!
    call jedetr('&&PBFLUI.TEMP.LBDA')
    call jedetr('&&PBFLUI.TEMP.KCAL')
    call jedetr('&&PBFLUI.TEMP.COND')
    call jedetr('&&PBFLUI.TEMP.GAMA')
    call jedetr('&&PBFLUI.TEMP.PASS')
    call jedetr('&&PBFLUI.TEMP.D')
    call jedetr('&&PBFLUI.TEMP.KI')
    call jedetr('&&PBFLUI.TEMP.HARM')
    call jedema()
end subroutine
