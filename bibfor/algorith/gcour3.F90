subroutine gcour3(resu, noma, coorn, lnoff, trav1,&
                  trav2, trav3, chfond, connex, grlt,&
                  liss, basfon, nbre, milieu,&
                  pair, ndimte, typdis, nomfis)
    implicit none
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! FONCTION REALISÉE:   DANS LE CADRE DE X-FEM
!
! 1.  POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), RINF, RSUP )
!
! 2.  PUIS ON  CALCULE LA DIRECTION DES CHAMPS THETA
!
!
! 3.  ENSUITE ON CALCULE LES CHAMPS THETA SUR TOUS LES NOEUDS DU
!     MAILLAGE
!
!     ------------------------------------------------------------------
! ENTREE:
!        RESU   : NOM DU CONCEPT RESULTAT
!        NOMA   : NOM DU CONCEPT MAILLAGE
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DU MAILLAGE
!        LNOFF  : NOMBRE DE NOEUDS DE GAMM0
!        CHFOND : NOMS DES NOEUDS DU FOND DE FISSURE
!        CONNEX : .TRUE.  : FOND DE FISSURE FERME
!                 .FALSE. : FOND DE FISSURE DEBOUCHANT
!        GRLT   : GRADIENT DE LA LEVEL-SET TANGENTE
!        TRAV1  : RINF
!        TRAV2  : RSUP
!        LISS   : TYPE DE LISSAGE
!        BASFON : BASE LOCALE AUX POINTS DU FOND DE FISSURE
!        NBRE   : DEGRE DES POLYNOMES DE LEGENDRE
!                     SINON 0
! SORTIE:
!                 LISTE DE CHAMPS_NO THETA
!        TRAV3  : MODULE(THETA)
!        MILIEU : .TRUE.  : ELEMENT QUADRATIQUE
!                 .FALSE. : ELEMENT LINEAIRE
!     ------------------------------------------------------------------
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscre.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: trav1, trav2, trav3, chfond, chamno, coorn
    character(len=24) :: basfon, liss
    character(len=19) :: cnsgt, grlt
    character(len=16) :: typdis
    character(len=8) :: resu, noma, nomfis
    character(len=6) :: kiord
    character(len=2) :: licmp(3)
!
    integer :: lnoff, iadrt1, iadrt2, iadrt3, itheta, iadrco, jmin
    integer :: imodu, nbre, iret, ndimte
    integer :: nbno, ifon, i, j, jresu, k, jgtl
!
    real(kind=8) :: xi1, yi1, zi1, xj1, yj1, zj1
    real(kind=8) :: xij, yij, zij, eps, d, grtx, grty, grtz
    real(kind=8) :: xm, ym, zm, xim, yim, zim, s, dmin, smin, xn, yn, zn
    real(kind=8) :: rii, rsi, alpha, valx, valy, valz, norm2
    real(kind=8) :: grtx0, grty0, grtz0, grtx1, grty1, grtz1
    character(len=19) :: cnsta, cnstet
    integer :: jcnsl, jstn, jstnl, stano
    data  licmp /'DX','DY','DZ'/
!
    aster_logical :: milieu, debug, pair, connex
!
!-----------------------------------------------------------------------
    integer :: iadrtt, jbas, kno
    real(kind=8) :: s0, s1
    real(kind=8), pointer :: cnsv(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    eps = 1.d-12
    debug=.false.
    milieu=.false.
!
    if(typdis.ne.'COHESIF') then
        call jeveuo(trav1, 'L', iadrt1)
        call jeveuo(trav2, 'L', iadrt2)
    endif
    call jeveuo(trav3, 'E', iadrt3)
!
    call jeveuo(coorn, 'L', iadrco)
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
    call jeveuo(chfond, 'L', ifon)
    call jeveuo(basfon, 'L', jbas)
!
!          -----------------------
!
!     RÉCUPÉRATION DES GRADIENTS DE LST
    cnsgt='&&GCOUR3.CNSGT'
    call cnocns(grlt, 'V', cnsgt)
    call jeveuo(cnsgt//'.CNSV', 'L', vr=cnsv)
    call jeveuo(cnsgt//'.CNSL', 'L', jgtl)
    if(typdis.eq.'COHESIF') then
        cnsta='&&GCOUR3.CNSTA'
        call cnocns(nomfis//'.STNO','V',cnsta)
        call jeveuo(cnsta//'.CNSV','L',jstn)
        call jeveuo(cnsta//'.CNSL','L',jstnl)
    endif
!
! ALLOCATION DES OBJETS POUR STOCKER LE CHAMP_NO THETA ET LA DIRECTION
! TYPE CHAM_NO ( DEPL_R) AVEC PROFIL NOEUD CONSTANT (3 DDL)
!
    if (liss .eq. 'LAGRANGE_REGU') then
        pair = .false.
        if (mod(lnoff,2) .eq. 1) ndimte = (lnoff+1)/2
        if (mod(lnoff,2) .eq. 0) then
            ndimte = 1+lnoff/2
            pair = .true.
            if (connex) call utmess('F', 'RUPTURE1_1')
        endif
    else if ((liss.eq.'LAGRANGE').or.(liss.eq.'LAGRANGE_NO_NO').or.(liss.eq.'MIXTE')) then
        ndimte = lnoff
    else
        ndimte = nbre + 1
    endif
!
    call wkvect(resu, 'V V K24', ndimte+1, jresu)
!
! BOUCLE GENERALE SUR LES NDIMTE+1 CHAMPS_NO A CREER
!
    cnstet = '&&GCOUR3.CNSTET'
    do k = 1, ndimte+1
!
        call codent(k, 'D0', kiord)
        chamno = resu(1:8)//'_CHAM'//kiord//'     '
        zk24(jresu+k-1) = chamno
        call exisd('CHAM_NO',chamno,iret)
        if (iret .eq. 1) then
            call detrsd('CHAM_NO',chamno)
        endif
        call cnscre(noma,'DEPL_R',3,licmp,'V',cnstet)
        call jeveuo(cnstet(1:19)//'.CNSL','E',jcnsl)
        call jeveuo(cnstet(1:19)//'.CNSV','E',itheta)
!
!
!       VOIR RÉFÉRENCE BOOK I (05/01/2004)
        if (k .ne. (ndimte+1)) then
!
            if (liss .eq. 'LAGRANGE_REGU') then
                kno = 2*k-1
                if ((k.eq. ndimte) .and. pair) then
                    kno = lnoff
                endif
                iadrtt = iadrt3 + (k-1)*lnoff + kno - 1
                zr(iadrtt) = 1.d0
                if (k .ne. 1) then
                    s0 = zr(ifon-1+4*(kno-1)+4)
                    s1 = zr(ifon-1+4*(kno-1-2)+4)
                    zr(iadrtt-1) = (zr(ifon-1+4*(kno-1-1)+4)-s1)/(s0- s1)
                endif
                if ((k.lt. (ndimte-1)) .or. (k.eq. (ndimte-1) .and. .not. pair)) then
                    s0 = zr(ifon-1+4*(kno-1)+4)
                    s1 = zr(ifon-1+4*(kno-1+2)+4)
                    zr(iadrtt+1) = (zr(ifon-1+4*(kno-1+1)+4)-s1)/(s0- s1)
                endif
                if (k .eq. (ndimte-1) .and. pair) then
                    zr(iadrtt+1) = 0.5d0
                endif
                if ((k.eq. ndimte) .and. pair) then
                    zr(iadrtt) = 0.5d0
                    zr(iadrtt-1) = 0.d0
                endif
                if ((k .eq. 1) .and. connex) then
                    iadrtt = iadrt3 + (k-1)*lnoff + lnoff - 1
                    s0 = zr(ifon-1+4*(lnoff-1)+4)
                    s1 = zr(ifon-1+4*(lnoff-1-2)+4)
                    zr(iadrtt) = (zr(ifon-1+4*(lnoff-1)+4)-s1)/(s0- s1)
                    zr(iadrtt-1) = (zr(ifon-1+4*(lnoff-1-1)+4)-s1)/(s0- s1)
                endif
                if ((k .eq. ndimte) .and. connex) then
                    iadrtt = iadrt3 + (k-1)*lnoff + 1 - 1
                    s0 = zr(ifon-1+4*(1-1)+4)
                    s1 = zr(ifon-1+4*(1-1+2)+4)
                    zr(iadrtt) = (zr(ifon-1+4*(1-1)+4)-s1)/(s0- s1)
                    zr(iadrtt+1) = (zr(ifon-1+4*(1-1+1)+4)-s1)/(s0- s1)
                endif
!
            else if ((liss.eq.'LAGRANGE').or.(liss.eq.'LAGRANGE_NO_NO').or.(liss.eq.'MIXTE')) then
                zr(iadrt3-1+(k-1)*lnoff+k) = 1.d0
                if ((k .eq. 1) .and. connex) then
                    iadrtt = iadrt3 + (k-1)*lnoff + lnoff - 1
                    zr(iadrtt) = 1.d0
                endif
                if ((k .eq. ndimte) .and. connex) then
                    iadrtt = iadrt3 + (k-1)*lnoff + 1 - 1
                    zr(iadrtt) = 1.d0
                endif
            endif
!         BOUCLE SUR LES NOEUDS M COURANTS DU MAILLAGE
!         POUR CALCULER PROJ(M)=N
!
            do i = 1, nbno
                zl(jcnsl-1+3*(i-1)+1)=.true.
                zl(jcnsl-1+3*(i-1)+2)=.true.
                zl(jcnsl-1+3*(i-1)+3)=.true.
                zr(itheta+(i-1)*3+1-1) = 0.d0
                zr(itheta+(i-1)*3+2-1) = 0.d0
                zr(itheta+(i-1)*3+3-1) = 0.d0
                if(typdis.eq.'COHESIF') then
                   if( .not.(zl(jstnl-1+i)) ) goto 500
                   stano = zi(jstn-1+i)
                   if(stano.eq.0) goto 500
                endif
                if (debug) write(6,*)'NOEUD MAIL',i
!             COORD DU NOEUD M DU MAILLAGE
                xm = zr(iadrco+(i-1)*3+1-1)
                ym = zr(iadrco+(i-1)*3+2-1)
                zm = zr(iadrco+(i-1)*3+3-1)
!             INITIALISATION
                dmin = r8maem()
                jmin = 0
                smin = 0.d0
!              BOUCLE SUR PT DE FONFIS (ALGO VOIR )
                do j = 1, lnoff-1
!               COORD PT I, ET J
                    xi1 = zr(ifon-1+4*(j-1)+1)
                    yi1 = zr(ifon-1+4*(j-1)+2)
                    zi1 = zr(ifon-1+4*(j-1)+3)
                    xj1 = zr(ifon-1+4*(j-1+1)+1)
                    yj1 = zr(ifon-1+4*(j-1+1)+2)
                    zj1 = zr(ifon-1+4*(j-1+1)+3)
!               VECTEUR IJ ET IM
                    xij = xj1-xi1
                    yij = yj1-yi1
                    zij = zj1-zi1
                    xim = xm-xi1
                    yim = ym-yi1
                    zim = zm-zi1
!               PARAM S (PRODUIT SCALAIRE...)
                    s = xij*xim + yij*yim + zij*zim
                    norm2 = xij*xij + yij *yij + zij*zij
                    s = s/norm2
!               SI N=P(M) SORT DU SEGMENT
                    if ((s-1) .ge. eps) then
                        s = 1.d0
                    endif
                    if (s .le. eps) then
                        s = 0.d0
                    endif
!               COORD DE N
                    xn = s*xij+xi1
                    yn = s*yij+yi1
                    zn = s*zij+zi1
!               DISTANCE MN
                    d = sqrt((xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+ (zn-zm)* (zn-zm))
                    if (d .lt. (dmin*(1-abs(r8prem())*1.d04))) then
                        dmin = d
                        jmin = j
                        smin = s
                    endif
                end do
!
                if(typdis.eq.'COHESIF') then
                    alpha = -0.5d0
                else
                    rii = (1-smin)*zr(iadrt1+jmin-1)+smin*zr(iadrt1+jmin+ 1-1)
                    rsi = (1-smin)*zr(iadrt2+jmin-1)+smin*zr(iadrt2+jmin+ 1-1)
                    alpha = (dmin-rii)/(rsi-rii)
                endif
!
                if ((abs(alpha-1).le.eps) .or. ((alpha-1).gt.0)) then
                    zr(itheta+(i-1)*3+1-1) = 0.d0
                    zr(itheta+(i-1)*3+2-1) = 0.d0
                    zr(itheta+(i-1)*3+3-1) = 0.d0
                else
                    if (zl(jgtl-1+(i-1)*3+1)) then
                        imodu = iadrt3+(k-1)*lnoff+jmin-1
                        grtx=cnsv((i-1)*3+1)
                        grty=cnsv((i-1)*3+2)
                        grtz=cnsv((i-1)*3+3)
                        valx =((1-smin) * zr(imodu) + smin * zr(imodu+&
                        1))*grtx
                        valy =((1-smin) * zr(imodu) + smin * zr(imodu+&
                        1))*grty
                        valz =((1-smin) * zr(imodu) + smin * zr(imodu+&
                        1))*grtz
                        if ((abs(alpha).le.eps) .or. (alpha.lt.0)) then
                            zr(itheta+(i-1)*3+1-1) = valx
                            zr(itheta+(i-1)*3+2-1) = valy
                            zr(itheta+(i-1)*3+3-1) = valz
                        else
                            zr(itheta+(i-1)*3+1-1) = (1-alpha)*valx
                            zr(itheta+(i-1)*3+2-1) = (1-alpha)*valy
                            zr(itheta+(i-1)*3+3-1) = (1-alpha)*valz
                        endif
!
!                 CORRECTION DE LA DIRECTION A L ORIGINE
                        if (jmin .eq. 1) then
                            if(typdis.ne.'COHESIF') then
                            grtx0=zr(jbas+4-1)* zr(imodu)
                            grty0=zr(jbas+5-1)* zr(imodu)
                            grtz0=zr(jbas+6-1)* zr(imodu)
                            grtx1=zr(jbas+(2-1)*6+4-1)* zr(imodu+1)
                            grty1=zr(jbas+(2-1)*6+5-1)* zr(imodu+1)
                            grtz1=zr(jbas+(2-1)*6+6-1)* zr(imodu+1)
                            valx =((1-smin) * grtx0 + smin * grtx1)
                            valy =((1-smin) * grty0 + smin * grty1)
                            valz =((1-smin) * grtz0 + smin * grtz1)
                            if ((abs(alpha).le.eps) .or. (alpha.lt.0)) then
                                zr(itheta+(i-1)*3+1-1) = valx
                                zr(itheta+(i-1)*3+2-1) = valy
                                zr(itheta+(i-1)*3+3-1) = valz
                            else
                                zr(itheta+(i-1)*3+1-1) = (1-alpha)* valx
                                zr(itheta+(i-1)*3+2-1) = (1-alpha)* valy
                                zr(itheta+(i-1)*3+3-1) = (1-alpha)* valz
                            endif
!
                            else if(typdis.eq.'COHESIF') then
                                valx = zr(jbas+(2-1)*6+4-1)* (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                valy = zr(jbas+(2-1)*6+5-1)* (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                valz = zr(jbas+(2-1)*6+6-1)* (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                zr(itheta+(i-1)*3+1-1) = valx
                                zr(itheta+(i-1)*3+2-1) = valy
                                zr(itheta+(i-1)*3+3-1) = valz
                            endif
                        endif
!
!                 CORRECTION DE LA DIRECTION A L ETREMITE
                        if (jmin .eq. (lnoff-1)) then
                            if(typdis.ne.'COHESIF') then
                            grtx0=zr(jbas+(lnoff-1-1)*6+4-1)* zr(&
                            imodu)
                            grty0=zr(jbas+(lnoff-1-1)*6+5-1)* zr(&
                            imodu)
                            grtz0=zr(jbas+(lnoff-1-1)*6+6-1)* zr(&
                            imodu)
                            grtx1=zr(jbas+(lnoff-1)*6+4-1)* zr(imodu+&
                            1)
                            grty1=zr(jbas+(lnoff-1)*6+5-1)* zr(imodu+&
                            1)
                            grtz1=zr(jbas+(lnoff-1)*6+6-1)* zr(imodu+&
                            1)
                            valx =((1-smin) * grtx0 + smin * grtx1)
                            valy =((1-smin) * grty0 + smin * grty1)
                            valz =((1-smin) * grtz0 + smin * grtz1)
                            if ((abs(alpha).le.eps) .or. (alpha.lt.0)) then
                                zr(itheta+(i-1)*3+1-1) = valx
                                zr(itheta+(i-1)*3+2-1) = valy
                                zr(itheta+(i-1)*3+3-1) = valz
                            else
                                zr(itheta+(i-1)*3+1-1) = (1-alpha)* valx
                                zr(itheta+(i-1)*3+2-1) = (1-alpha)* valy
                                zr(itheta+(i-1)*3+3-1) = (1-alpha)* valz
                            endif
                            else if(typdis.eq.'COHESIF') then
                                valx = zr(jbas+(lnoff-1-1)*6+4-1)*&
                                      (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                valy = zr(jbas+(lnoff-1-1)*6+5-1)*&
                                      (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                valz = zr(jbas+(lnoff-1-1)*6+6-1)*&
                                      (zr(imodu)*(1-smin)+smin*zr(imodu+1))
                                zr(itheta+(i-1)*3+1-1) = valx
                                zr(itheta+(i-1)*3+2-1) = valy
                                zr(itheta+(i-1)*3+3-1) = valz
                            endif
                        endif
!
                    endif
                endif
500         continue
            end do
! on transforme le cham_no_s en cham_no
          call cnscno(cnstet,' ','OUI','V',chamno,'F',iret)
          call detrsd('CHAM_NO_S',cnstet)
        endif
!
    end do
!
    call jedema()
!
end subroutine
