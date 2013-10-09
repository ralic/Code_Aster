subroutine accep1(modmec, ligrmo, nbm, dir, yang)
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
!     OPERATEUR PROJ_SPEC_BASE
!     PROJECTION D UN OU PLUSIEURS SPECTRES DE TURBULENCE SUR UNE BASE
!     MODALE PERTURBEE PAR PRISE EN COMPTE DU COUPLAGE FLUIDE STRUCTURE
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mecact.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbm, i
    integer :: iret, ilime, inoli, ngrel, ipg, n1, jlgrf
    integer :: ncham, icham, nn, nbelto, nbelgr, ntail, ialiel
    integer :: igr, ima, ii, iel, ive, itab, imo
    real(kind=8) :: dir(3, 3), v1, v2, v3, w1, w2, w3, ref1, ref2, ref3, refer
    real(kind=8) :: rayon, rayon2, haut, rap1, rap2
    character(len=7) :: incr, ielem, imode
    character(len=8) :: vetel, lpain(3), lpaout(1), modele, modmec, partit, k8b
    character(len=16) :: option
    character(len=19) :: nomcha, chgeom, matas, chharm
    character(len=24) :: ligrmo, lchin(3), lchout(1)
    logical :: yang
!
!-----------------------------------------------------------------------
    call jemarq()
!
    option = 'ACCEPTANCE'
    call getvid(' ', 'MODELE_INTERFACE', scal=modele, nbret=iret)
    if (iret .le. 0) then
!       --- PAS DE MODELE D'INTERFACE, ALORS RECUPERER LE MODELE MECA
!           GLOBAL A PARTIR DE LA MATRICE DE RIGIDITE ASSEMBLEE QUI
!           EST REFERENCEE DANS LE .REFD DE LA BASE MODALE MODE_MECA
        call getvid(' ', 'MODE_MECA', scal=k8b, nbret=iret)
        if (iret .gt. 0) then
            call rsexch(' ', modmec, 'DEPL', 1, nomcha,&
                        iret)
            call dismoi('REF_RIGI_PREM', modmec, 'RESU_DYNA', repk=matas)
            call jeveuo(matas//'.LIME', 'L', ilime)
            call jeveuo(zk24(ilime)(1:8)//'.ME001     .NOLI', 'L', inoli)
            modele = zk24(inoli) (1:8)
        else
!         --- DEFORMEES MODALES PAR DES CHAM_NO MAIS AUCUNE INFORMATION
!             N'EST PRESENTE SUR LE MODELE EF...
!             CE BLINDAGE EST REDONDANT AVEC LES REGLES DU CATALOGUE
            ASSERT(.false.)
        endif
    endif
!
!     --- SCRUTER LES MOTS CLE TOUT/GROUP_MA/MAILLE POUR CREER
!         UN LIGREL "REDUIT" DANS LIGRMO
    call exlima(' ', 0, 'V', modele, ligrmo)
    if (ligrmo(1:8) .ne. modele) then
!       --- RENOMMER LA SD_LIGREL OBTENUE
        call copisd('LIGREL', 'V', ligrmo, '&&ACCEP1.MODELE         ')
        call detrsd('LIGREL', ligrmo)
        ligrmo = '&&ACCEP1.MODELE         '
        modele = ligrmo(1:8)
    endif
!
    call dismoi('PARTITION', ligrmo, 'LIGREL', repk=partit)
    if (partit .ne. ' ') then
        call utmess('F', 'CALCULEL_25', sk=ligrmo)
    endif
!
! CALCULS ELEMENTAIRES
    call jeveuo(ligrmo(1:19)//'.LGRF', 'L', jlgrf)
    chgeom = zk8(jlgrf-1+1)//'.COORDO'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PACCELR'
    lpain(3) = 'PNUMMOD'
    lpaout(1) = 'PVECTUR'
! RECHERCHE SI UN CHAMNO A ETE DONNE
    call getvid(' ', 'CHAM_NO', nbval=0, nbret=ncham)
    if (ncham .ne. 0) then
        ncham = -ncham
        call wkvect('&&ACCEP1.VEC', 'V V K8', ncham, icham)
        call getvid(' ', 'CHAM_NO', nbval=ncham, vect=zk8(icham), nbret=nn)
    endif
! BOUCLE SUR LES MODES FORMATIONS DES VECTEURS ELEMENTAIRES
    do i = 1, nbm
        call codent(i, 'D0', incr)
        vetel = '&&V.M'//incr(5:7)
        lchout(1) = vetel//'.VE000'
        if (ncham .eq. 0) then
            call rsexch(' ', modmec, 'DEPL', i, nomcha,&
                        iret)
        else
            if (i .le. ncham) nomcha = zk8(icham+i-1)
        endif
        lchin(2) = nomcha//'.VALE'
        call codent(1, 'D0', lchout(1) (12:14))
        chharm = '&&ACCEP1.NUME_HARM'
        call mecact('V', chharm, 'MODELE', modele, 'NUMMOD',&
                    ncmp=1, nomcmp='NUM', si=i)
        lchin(3) = chharm
        call calcul('S', option, ligrmo, 3, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call detrsd('CARTE', chharm)
    end do
    if (ncham .gt. 0) call jedetr('&&ACCEP1.VEC')
!
!
!  --- CREATION D' UN TABLEAU CONTENANT LES INFORMATIONS SUIVANTES :
!      POUR CHAQUE POINT DE GAUSS DE CHAQUE ELEMENT : 6 VALEURS
!      1: LA PRESSION     2,3,4: LES COORDONNEES DES POINTS DE GAUSS
!      POUR AU-YANG : 5: LA HAUTEUR DU POINT   6: L'ANGLE DU POINT
!      POUR LES AUTRES METHODES 5: 0. ET 6: 0. (NON UTILISEES)
!
    ngrel = nbgrel(ligrmo)
!
    nbelto = 0
    do igr = 1, ngrel
        nbelgr = nbelem(ligrmo,igr)
        nbelto = nbelto + nbelgr
    end do
!
! TAILLE DU TABLEAU
!          NTAIL=16*NBELTO*NBM
    ntail = 24*nbelto*nbm + 1
    call wkvect('&&GROTAB.TAB', 'V V R', ntail, itab)
! NOMBRE D'ELEMENTS PAR MODE
!
! CONSTITUTION D'UN TABLEAU CONTENANT COORDONNEES DES PTS DE GAUSS
! AINSI QUE LA VALEUR DU MODE
    ii = 1
    do imo = 1, nbm
        imode = 'CHBIDON'
        call codent(imo, 'D0', imode)
        do igr = 1, ngrel
            nbelgr = nbelem(ligrmo,igr)
            call jeveuo(jexnum(ligrmo(1:19)//'.LIEL', igr), 'L', ialiel)
            do iel = 1, nbelgr
                ima = zi(ialiel-1+iel)
                ielem = 'BID'
                call codent(ima, 'D0', ielem)
                call jeveuo('&&329.M'//imode//'.EL'//ielem, 'L', ive)
                call jelira('&&329.M'//imode//'.EL'//ielem, 'LONMAX', n1)
                do ipg = 1, n1
                    zr(itab+ii-1) = zr(ive+ipg-1)
                    ii = ii + 1
                    if (mod(ii,6) .eq. 5) then
                        if (.not.yang) then
                            zr(itab+ii-1) = 0.d0
                            zr(itab+ii) = 0.d0
                            ii = ii + 2
                        else
                            v1 = zr(itab+ii-4) - dir(1,2)
                            v2 = zr(itab+ii-3) - dir(2,2)
                            v3 = zr(itab+ii-2) - dir(3,2)
                            haut = v1*dir(1,1) + v2*dir(2,1) + v3*dir( 3,1)
                            w1 = v1 - haut*dir(1,1)
                            w2 = v2 - haut*dir(2,1)
                            w3 = v3 - haut*dir(3,1)
                            zr(itab+ii-1) = haut
                            ii = ii + 1
                            rayon2 = w1*w1 + w2*w2 + w3*w3
                            if (rayon2 .le. 0.d0) then
                                call utmess('F', 'MODELISA_6')
                            endif
                            if (ii .eq. 6) then
                                refer = rayon2
                                rayon = sqrt(rayon2)
                                ref1 = w1
                                ref2 = w2
                                ref3 = w3
                                zr(itab+ntail-1) = rayon
                                zr(itab+5) = 0.d0
                                ii = 7
                            else
                                if (abs(rayon2-refer) .gt. 1.d-3) then
                                    call utmess('F', 'MODELISA_6')
                                endif
                                rap1 = (ref2*w3-ref3*w2)*dir(1,1) + (ref3*w1-ref1*w3)*dir(2,1) + &
                                       &(ref1*w2- ref2*w1)*dir(3,1)
                                rap2 = ref1*w1 + ref2*w2 + ref3*w3
                                zr(itab+ii-1) = atan2(rap1,rap2)
                                ii = ii + 1
                            endif
                        endif
                    endif
                end do
            end do
        end do
    end do
!
    call jedema()
end subroutine
