subroutine xddlim(modele, motcle, nomn, ino, valimr,&
                  valimc, valimf, fonree, icompt, lisrel,&
                  ndim, direct, jnoxfv, ch1, ch2,&
                  ch3, cnxinv, mesh, hea_no)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8pi.h"
#include "asterfort/assert.h"
#include "asterfort/afrela.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/xcalc_heav.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xddlimf.h"
!
    integer :: ino, icompt, ndim, jnoxfv
    real(kind=8) :: valimr, direct(3)
    character(len=4) :: fonree
    character(len=8) :: modele, nomn, valimf, motcle
    character(len=8), intent(in) :: mesh
    character(len=19) :: lisrel, ch1, ch2, ch3, cnxinv, hea_no
! ---------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!
!      TRAITEMENT DE DDL_IMPO SUR UN NOEUD X-FEM
!             (POUR MOTCLE = DX, DY ,DZ)
!      TRAITEMENT DE DDL_IMPO SUR UN NOEUD HM-XFEM
!             (POUR MOTCLE = DX, DY, DZ ET/OU PRE1)
!      TRAITEMENT DE FACE_IMPO SUR UN NOEUD X-FEM
!             (POUR DNOR OU DTAN : MOTCLE = DEPL )
!
! IN  MODELE : NOM DE L'OBJET MODELE ASSOCIE AU LIGREL DE CHARGE
! IN  MOTCLE : NOM DE LA COMPOSANTE DU DEPLACEMENT/PRESSION A IMPOSER
! IN  NOMN   : NOM DU NOEUD INO OU EST EFFECTUE LE BLOCAGE
! IN  INO    : NUMERO DU NOEUD OU EST EFFECTUE LE BLOCAGE
! IN  VALIMR : VALEUR DE BLOCAGE SUR CE DDL (FONREE = 'REEL')
! IN  VALIMC : VALEUR DE BLOCAGE SUR CE DDL (FONREE = 'COMP')
! IN  VALIMF : VALEUR DE BLOCAGE SUR CE DDL (FONREE = 'FONC')
! IN  FONREE : AFFE_CHAR_XXXX OU AFFE_CHAR_XXXX_F
! IN  NDIM
! IN  MESH   : NOM DU MAILLAGE
!
! IN/OUT
!     ICOMPT : "COMPTEUR" DES DDLS AFFECTES REELLEMENT
!     LISREL : LISTE DE RELATIONS AFFECTEE PAR LA ROUTINE
!
!
!
!
    integer :: nbxcmp
    parameter  (nbxcmp=60)
    integer :: ier, stano(4), jstnol,  jstnod, nrel
    integer ::  jlsnl, jlsnd,  jlstl, jlstd
    integer ::  jfisnl, jfisnd, nfh, ifh
    integer ::  i, j, nterm, irel, dimens(nbxcmp), ifiss, nfiss
    integer ::  nbno, nbmano, adrma, ima, numa, nbnoma, nuno, nuno2
    integer ::  jconx2,  iad, fisno(4)
    integer ::  jheavnl, jheavnd, ncompn, heavn(5), hea_se
    real(kind=8) :: r, theta(2), he(2, 4), t, coef(nbxcmp), sign
    real(kind=8) :: lsn(4), lst(4), minlsn, maxlsn, lsn2
    character(len=8) :: ddl(nbxcmp), noeud(nbxcmp), noma
    character(len=1) :: axes(3)
    character(len=19) :: ch4
    complex(kind=8) :: cbid, valimc
    character(len=1) :: ch
    aster_logical :: class
    integer, pointer :: nunotmp(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
    integer, pointer :: connex(:) => null(), ihea_no(:) => null()
    integer, pointer :: fisnv(:) => null()
    real(kind=8), pointer :: lsnv(:) => null()
    real(kind=8), pointer :: lstv(:) => null()
    integer, pointer :: stnov(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUP DU NUMERO LOCAL NUMO DU NOEUD INO DANS LA MAILLE X-FEM NUMA
    numa = zi(jnoxfv-1+2*(ino-1)+1)
    nuno = zi(jnoxfv-1+2*(ino-1)+2)
    axes(1) = 'X'
    axes(2) = 'Y'
    axes(3) = 'Z'
!
!       REMARQUE : FAIRE DES CALL JEVEUO AU COEUR DE LA BOUCLE SUR
!      LES NOEUDS ET SUR LES DDLS BLOQUES N'EST PAS OPTIMAL DU POINT
!      DE VUE DES PERFORMANCES, MAIS A PRIORI, CA NE DEVRAIT PAS ETRE
!      POUR BEAUCOUP DE NOEUDS
    call jeveuo(ch1//'.CESV', 'L', vi=stnov)
    call jeveuo(ch1//'.CESL', 'L', jstnol)
    call jeveuo(ch1//'.CESD', 'L', jstnod)
    call jeveuo(ch2//'.CESV', 'L', vr=lsnv)
    call jeveuo(ch2//'.CESL', 'L', jlsnl)
    call jeveuo(ch2//'.CESD', 'L', jlsnd)
    call jeveuo(ch3//'.CESV', 'L', vr=lstv)
    call jeveuo(ch3//'.CESL', 'L', jlstl)
    call jeveuo(ch3//'.CESD', 'L', jlstd)
!
! --- NOMBRE DE FISSURES VUES PAR LA MAILLE
    nfiss = zi(jstnod-1+5+4*(numa-1)+2)
!
    if (nfiss .gt. 1) then
        ch4 = '&&XDDLIM.CHS4'
        call celces(modele//'.FISSNO', 'V', ch4)
        call jeveuo(ch4//'.CESV', 'L', vi=fisnv)
        call jeveuo(ch4//'.CESL', 'L', jfisnl)
        call jeveuo(ch4//'.CESD', 'L', jfisnd)
! --- NOMBRE DE DDLS HEAVISIDES DANS LA MAILLE
        nfh = zi(jfisnd-1+5+4*(numa-1)+2)
        do i = 1, nfh
            call cesexi('S', jfisnd, jfisnl, numa, nuno,&
                        i, 1, iad)
            fisno(i) = fisnv(iad)
        end do
    endif
    do ifiss = 1, nfiss
        call cesexi('S', jstnod, jstnol, numa, nuno,&
                    ifiss, 1, iad)
        stano(ifiss)=stnov(iad)
        call cesexi('S', jlsnd, jlsnl, numa, nuno,&
                    ifiss, 1, iad)
        lsn(ifiss) = lsnv(iad)
        call cesexi('S', jlstd, jlstl, numa, nuno,&
                    ifiss, 1, iad)
        lst(ifiss) = lstv(iad)
    end do
!
! --- RECUPERATION DE LA DEFINITION DES DDLS HEAVISIDES
    hea_se=-99
    heavn(1:5)=0
    if (stano(1) .eq. 1 .or. stano(1) .eq. 3) then
      call jeveuo(hea_no//'.CESV', 'L', vi=ihea_no)
      call jeveuo(hea_no//'.CESL', 'L', jheavnl)
      call jeveuo(hea_no//'.CESD', 'L', jheavnd)
      ncompn = zi(jheavnd-1+5+4*(numa-1)+3)
!      ASSERT(ncompn.eq.5)
      do i = 1, ncompn
        call cesexi('S', jheavnd, jheavnl, numa, nuno,&
                   1, i, iad)
        heavn(i) = ihea_no(iad)
      end do
    endif
!
! --- IDENTIFICATIOND DES CAS A TRAITER :
! --- SI LA RELATION CINEMATIQUE EST IMPOSEE PAR DES VALEURS REELLES ET
! --- SI NOEUD SUR LES LEVRES ET CONNECTÉ À DES NOEUDS (APPARTENANT AU
! --- GROUPE AFFECTÉ) DE PART ET D'AUTRE DE LA LEVRE : 2 RELATIONS
! --- SINON IL FAUT IMPOSER QUE D'UN SEUL COTÉ        : 1 RELATION
    if (nfiss .eq. 1) then
        if (lsn(1) .eq. 0.d0 .and. lst(1) .lt. 0.d0) then
            minlsn = r8maem()
            maxlsn = -1*r8maem()
! ---     RECUPERATION DE LA LISTE DES NOEUDS AFFECTÉS PAR LA CONDITION
            call jeexin('&&CADDLI.LIST_NODE', ier)
            if (ier .ne. 0) then
                call jeveuo('&&CADDLI.LIST_NODE', 'L', vi=nunotmp)
                call jelira('&&CADDLI.LIST_NODE', 'LONMAX', nbno)
            endif
            call jeexin('&&CAFACI.LIST_NODE', ier)
            if (ier .ne. 0) then
                call jeveuo('&&CAFACI.LIST_NODE', 'L', vi=nunotmp)
                call jelira('&&CAFACI.LIST_NODE', 'LONMAX', nbno)
            endif
            call jeexin('&&CAAREI.LIST_NODE', ier)
            if (ier .ne. 0) then
                call jeveuo('&&CAAREI.LIST_NODE', 'L', vi=nunotmp)
                call jelira('&&CAAREI.LIST_NODE', 'LONMAX', nbno)
            endif
! ---     RECUPERATION DU NOM DU MAILLAGE :
            call jeveuo(modele//'.MODELE    .LGRF', 'L', vk8=lgrf)
            noma = lgrf(1)
! ---     RECUPERATION DES MAILLES CONTENANT LE NOEUD
            call jeveuo(noma//'.CONNEX', 'L', vi=connex)
            call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
            call jelira(jexnum(cnxinv, ino), 'LONMAX', nbmano)
            call jeveuo(jexnum(cnxinv, ino), 'L', adrma)
! ---     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD
            do ima = 1, nbmano
                numa = zi(adrma-1 + ima)
                nbnoma = zi(jconx2+numa) - zi(jconx2+numa-1)
! ---       BOUCLE SUR LES NOEUDS DE LA MAILLE
! ---       ATTENTION ON NE PREND EN COMPTE QUE LES MAILLES LINEAIRES !
                do i = 1, nbnoma
                    nuno = connex(zi(jconx2+numa-1)+i-1)
! ---         ON REGARDE SI LE NOEUD APPARTIENT AU GRP DE NOEUD AFFECTÉ
                    do j = 1, nbno
                        nuno2 = nunotmp(j)
                        if (nuno2 .eq. nuno) then
                            call cesexi('C', jlsnd, jlsnl, numa, i,&
                                        1, 1, iad)
                            if (iad .le. 0) goto 110
                            lsn2 = lsnv(iad)
!                  LSN2 = ZR(JLSN-1+NUNO)
                            if (lsn2 .lt. minlsn) minlsn=lsn2
                            if (lsn2 .gt. maxlsn) maxlsn=lsn2
                            goto 110
                        endif
                    end do
110                 continue
                end do
            end do
!
            if ((minlsn.eq.0.d0) .and. (maxlsn.gt.0.d0)) then
! ---       ON AFFECTE LA RELATION UNIQUEMENT SUR LA PARTIE MAITRE
                nrel = 1
                theta(1) = r8pi()
                he(1,1) = 1.d0
            else if ((minlsn.lt.0.d0).and.(maxlsn.eq.0.d0)) then
! ---       ON AFFECTE LA RELATION UNIQUEMENT SUR LA PARTIE ESCLAVE
                nrel = 1
                theta(1) = r8pi()
                he(1,1) = -1.d0
            elseif (((minlsn.lt.0.d0).and.(maxlsn.gt.0.d0)) .or.(&
            nbno.eq.0)) then
! ---       ON AFFECTE LA RELATION SUR LES DEUX PARTIES
                nrel = 2
                theta(1) = r8pi()
                theta(2) = -r8pi()
                he(1,1) = 1.d0
                he(2,1) = -1.d0
            else
! ---       SI NOEUD ISOLE, ON AFFECTE RIEN POUR L'INSTANT
                goto 888
            endif
        else
            nrel = 1
            he(1,1) = sign(1.d0,lsn(1))
            theta(1) = he(1,1)*abs(atan2(lsn(1),lst(1)))
        endif
    else if (nfiss .gt. 1) then
        nrel = 1
        do ifh = 1, nfh
! --- ON NE PREND PAS ENCORE EN COMPTE LE CAS OU ON PASSE PAR UN NOEUD POUR
! --- LES ELEMENTS MULTI-HEAVISIDE
            if (lsn(fisno(ifh)) .eq. 0) goto 888
            he(1,ifh) = 0.d0
        end do
    endif
!
    do i = 1, nbxcmp
        dimens(i)= 0
        noeud(i) = nomn
    end do
!
    if (nrel .eq. 2 .and. fonree .eq. 'REEL') then
       call utmess('A', 'XFEM_22', sk=nomn)
    endif
!
    if (fonree.eq.'FONC' .and. nfiss .eq.1 .and. stano(1) .eq. 1) then
! --- SI LA RELATION CINEMATIQUE EST IMPOSEE PAR UNE FONCTION DE L'ESPACE
       call xddlimf(modele, ino, cnxinv, jnoxfv, motcle,&
                    ch2, ndim, lsn, lst, valimr, valimf, valimc,&
                    fonree, lisrel, nomn, direct, class, mesh,&
                    hea_no)
    endif
!     IMPOSITION DES CONDITIONS CINEMATIQUE "TOTALES" (DDL_CLASS +/- DDL_ENR)
    if ((fonree.eq.'REEL') .or. (nfiss.gt.1) .or. class .or. (stano(1).ne.1)) then
       do i = 1, nbxcmp
           dimens(i)= 0
           noeud(i) = nomn
       end do
! --- BOUCLE SUR LES RELATIONS
       do irel = 1, nrel
!
! --- CALCUL DU SOUS DOMAINE CORRESPONDANT A CHAQUE RELATION LINEAIRE
        if (nfiss.eq.1) hea_se=xcalc_code(1,he_real=[he(irel,1)])
!
!       CALCUL DES COORDONNÉES POLAIRES DU NOEUD (R,T)
        r = sqrt(lsn(1)**2+lst(1)**2)
        t = theta(irel)
!
!       CAS FACE_IMPO DNOR OU DTAN
        if (motcle(1:8) .eq. 'DEPL    ') then
!
            i = 0
            do j = 1, ndim
!
!           COEFFICIENTS ET DDLS DE LA RELATION
                i = i+1
                ddl(i) = 'D'//axes(j)
                coef(i)=direct(j)
!
                if (nfiss .eq. 1) then
                    if (stano(1) .eq. 1 .or. stano(1) .eq. 3) then
                        i = i+1
                        ddl(i) = 'H1'//axes(j)
                        coef(i)=xcalc_heav(heavn(1),hea_se,heavn(5))*direct(j)
                    endif
!
                    if (stano(1) .eq. 2 .or. stano(1) .eq. 3) then
                        i = i+1
                        ddl(i) = 'E1'//axes(j)
                        coef(i)=sqrt(r)*sin(t/2.d0)*direct(j)
                        i = i+1
                        ddl(i) = 'E2'//axes(j)
                        coef(i)=sqrt(r)*cos(t/2.d0)*direct(j)
                        i = i+1
                        ddl(i) = 'E3'//axes(j)
                        coef(i)=sqrt(r)*sin(t/2.d0)*sin(t)*direct(j)
                        i = i+1
                        ddl(i) = 'E4'//axes(j)
                        coef(i)=sqrt(r)*cos(t/2.d0)*sin(t)*direct(j)
                    endif
                else
                    do ifh = 1, nfh
                        if (stano(fisno(ifh)) .eq. 1) then
                            i = i+1
                            call codent(ifh, 'G', ch)
                            ddl(i) = 'H'//ch//axes(j)
                            coef(i)=he(irel,ifh)*direct(j)
                        endif
                    end do
                endif
            end do
!       CAS DDL_IMPO DX DY DZ (ET/OU PRE1 => POUR HM-XFEM ONLY)
        elseif (motcle.eq.'DX'.or.motcle.eq.'DY'.or.motcle.eq.'DZ') then
!         COEFFICIENTS ET DDLS DE LA RELATION
              ddl(1) = 'D'//motcle(2:2)
              coef(1)=1.d0
              i = 1
              if (nfiss .eq. 1) then
                if (stano(1) .eq. 1 .or. stano(1) .eq. 3) then
                    i = i+1
                    ddl(i) = 'H1'//motcle(2:2)
                    coef(i)=xcalc_heav(heavn(1),hea_se,heavn(5))
                endif
                if (stano(1) .eq. 2 .or. stano(1) .eq. 3) then
                    i = i+1
                    ddl(i) = 'E1'//motcle(2:2)
                    coef(i)=sqrt(r)*sin(t/2.d0)
                    i = i+1
                    ddl(i) = 'E2'//motcle(2:2)
                    coef(i)=sqrt(r)*cos(t/2.d0)
                    i = i+1
                    ddl(i) = 'E3'//motcle(2:2)
                    coef(i)=sqrt(r)*sin(t/2.d0)*sin(t)
                    i = i+1
                    ddl(i) = 'E4'//motcle(2:2)
                    coef(i)=sqrt(r)*cos(t/2.d0)*sin(t)
                endif
              else
                do ifh = 1, nfh
                    if (stano(fisno(ifh)) .eq. 1) then
                        i = i+1
                        call codent(ifh, 'G', ch)
                        ddl(i) = 'H'//ch//motcle(2:2)
                        coef(i)=he(irel,ifh)
                    endif
                end do
              endif
        elseif (motcle.eq.'PRE1') then
!           COEFFICIENTS ET DDLS DE LA RELATION
              ddl(1) = 'PRE1'
              coef(1) = 1.d0
              i=1
              if (nfiss.eq.1) then
                if (stano(1).eq.1.or.stano(1).eq.3) then
                  i = i + 1
                  ddl(i) = 'H'//motcle(1:4)
                  coef(i) =xcalc_heav(heavn(1),hea_se,heavn(5))
                endif
              endif
        endif
        nterm = i
           call afrela(coef, [cbid], ddl, noeud, dimens,&
                       [0.d0], nterm, valimr, valimc, valimf,&
                       'REEL', fonree, '12', 0.d0, lisrel)
       end do
!
    endif
!
    icompt = icompt + 1
!
888 continue
!
    if (nfiss .gt. 1) call detrsd('CHAM_ELEM_S', ch4)
!
    call jedema()
end subroutine
