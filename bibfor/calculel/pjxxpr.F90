subroutine pjxxpr(resu1, resu2, moa1, moa2, corres,&
                  base, noca, method, xfem)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
! --------------------------------------------------------------------------------------------------
! BUT :
!  PROJETER LES CHAMPS CONTENUS DANS LA SD RESU1
!  SUR LE MODELE (OU MAILLAGE) MOA2
!  ET CREER UNE NOUVELLE SD RESU2 DE MEME TYPE QUE RESU1
!
!  IN/JXIN  RESU1   K8  : NOM DE LA SD_RESULTAT A PROJETER
!  IN/JXOUT RESU2   K8  : NOM DE LA SD_RESULTAT RESULTAT
!  IN/JXIN  MOA1  K8  : NOM DU MODELE (OU MAILLAGE) ASSOCIE A RESU1
!  IN/JXIN  MOA2  K8  : NOM DU MODELE (OU MAILLAGE) ASSOCIE A RESU2
!  IN/JXIN  CORRES  K16 : NOM DE LA SD CORRESP_2_MAILLA
!
!  RESTRICTIONS :
!   1- ON TRAITE SYSTEMATIQUEMENT TOUS LES NUMEROS D'ORDRE
!   2- ON NE TRAITE CORRECTEMENT QUE LES EVOL_XXX (INST)
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pjspma.h"
#include "asterfort/pjxxch.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutc4.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/vpcrea.h"
#include "asterfort/wkvect.h"
#include "asterfort/pjxfem.h"
!
! --------------------------------------------------------------------------------------------------
    character(len=1) :: base
    character(len=8) :: resu1, resu2, moa1, moa2, noca
    character(len=16) :: corres
    character(len=19) :: method
    aster_logical, optional :: xfem
! --------------------------------------------------------------------------------------------------
!
    integer :: ibid, ie, iret, jordr, nbordr, i, iordr, tmod(1)
    integer :: iains1, iains2, nbsym, isym, ico, ind, nbmax
    integer :: iexi, jpara, ier, inume
    parameter (nbmax=50)
    integer :: ipar, ipar1, ipar2
    aster_logical :: acceno, lxfem, lpjxfem
    real(kind=8) :: r8b, prec, inst
    complex(kind=8) :: c16b
    character(len= 1) :: typerr
    character(len= 4) :: tychv, tych
    character(len= 8) :: kb, ma1, ma2, nume, prol0, k8b, typ1, typ2, crit, mo2
    character(len=16) :: nomsym(200), k16b, nomcmd, typres
    character(len=19) :: ch1, ch2, prfchn, ligrel, prfch2, noms2, kpar(nbmax)
    character(len=24) :: valk(3), noojb
!
    character(len=24), pointer :: pjxx_k1(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    k8b = ' '
    tychv = ' '
    call getres(k8b, k16b, nomcmd)
    if (present(xfem)) then
      lxfem=xfem
    else
      lxfem=.false._1
    endif
!     -- CALCUL DE MA1, MA2, LIGREL :
    call jeexin(moa1//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        call dismoi('NOM_MAILLA', moa1, 'MODELE', repk=ma1)
    else
        ma1=moa1
    endif
!
    call jeexin(moa2//'.MODELE    .REPE', iexi)
    if (iexi .gt. 0) then
        call dismoi('NOM_MAILLA', moa2, 'MODELE', repk=ma2)
        mo2=moa2
        ligrel = mo2//'.MODELE'
!
    else
        ma2=moa2
        mo2=' '
        ligrel=' '
    endif
!
!
!
    if (nomcmd .eq. 'DEPL_INTERNE') then
!       ON NE TRAITE QUE LE CHAMP DEPL
        nbsym = 1
        nomsym(1) = 'DEPL'
        call rsorac(resu1, 'LONUTI', 0, r8b, kb,&
                    c16b, r8b, kb, tmod, 1,&
                    ibid)
        nbordr=tmod(1)
        if (nbordr .eq. 0) then
            call utmess('F', 'CALCULEL4_62', sk=resu1)
        endif
!
        call wkvect('&&PJXXPR.NUME_ORDRE', 'V V I', nbordr, jordr)
!
        call rsorac(resu1, 'TOUT_ORDRE', 0, r8b, kb,&
                    c16b, r8b, kb, zi(jordr), nbordr,&
                    ibid)
!
        prol0 = 'OUI'
!
    else
        call jeveuo(corres//'.PJXX_K1', 'L', vk24=pjxx_k1)
        if (method(1:10) .ne. 'SOUS_POINT') then
            if (pjxx_k1(2) .ne. ma2) then
                call utmess('F', 'CALCULEL4_60')
            endif
        else
            if ( method.eq.'SOUS_POINT_RIGI' ) then
                call utmess('F', 'CALCULEL5_28')
        endif
        endif
!
        call rsutc4(resu1, ' ', 1, 200, nomsym,&
                    nbsym, acceno)
        ASSERT(nbsym.gt.0)
!
        call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ier)
        call getvtx(' ', 'TYPE_CHAM', scal=tychv, nbret=ibid)
!
!
!     1- CREATION DE LA SD RESULTAT : RESU2
!     ------------------------------------
        call getvr8(' ', 'PRECISION', scal=prec, nbret=ie)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=ie)
        call rsutnu(resu1, ' ', 0, '&&PJXXPR.NUME_ORDRE', nbordr,&
                    prec, crit, iret)
        if (iret .ne. 0) then
            call utmess('F', 'CALCULEL4_61', sk=resu1)
        endif
        if (nbordr .eq. 0) then
            call utmess('F', 'CALCULEL4_62', sk=resu1)
        endif
        call jeveuo('&&PJXXPR.NUME_ORDRE', 'L', jordr)
    endif
!
    noms2 = resu2
    call jeexin(noms2//'.DESC', iret)
    if (iret .eq. 0) then
        call gettco(resu1, typres)
        call rscrsd(base, resu2, typres, nbordr)
    endif
!
!   DANS LE CAS DES CONCEPTS TYPE MODE_MECA ON TESTE LA PRESENCE
!   DES MATRICES AFIN DE RECUPERER LA NUMEROTATION SOUS-JACENTE
    prfch2 = '12345678.00000.NUME'
    if (nomcmd .eq. 'DEPL_INTERNE') then
!
    else
!       ON ESSAYE DE RECUPERER LA NUMEROTATION IMPOSEE
        call getvid(' ', 'NUME_DDL', scal=nume, nbret=ier)
        if (ier .ne. 0) then
            prfch2 = nume(1:8)//'      .NUME'
        endif
    endif
!
!   2- ON CALCULE LES CHAMPS RESULTATS :
!   ------------------------------------
    ico = 0
    do isym = 1,nbsym
!
        if (prfch2 .ne. '12345678.00000.NUME') then
!           ON PREND LA NUMEROTATION IMPOSEE
            prfchn = prfch2
        else
!           ON DEFINIT UNE NUMEROTATION 'BIDON"
            noojb = '12345678.00000.NUME.PRNO'
            call gnomsd(' ', noojb, 10, 14)
            prfchn = noojb(1:19)
        endif
!
        do i = 1,nbordr
            iordr = zi(jordr+i-1)
            call rsexch(' ', resu1, nomsym(isym), iordr, ch1,&
                        iret)
            if (iret .gt. 0) goto 20
!           PROJECTION DU CHAMP SI POSSIBLE :
            call rsexch(' ', resu2, nomsym(isym), iordr, ch2,&
                        iret)
!           VERIF ULTIME DANS LE CAS XFEM SI LE CHAMP EST NODAL :
            lpjxfem=.false._1
            if (lxfem) then
               call dismoi('TYPE_CHAMP', ch1, 'CHAMP', repk=tych)
               if (tych.eq.'NOEU'.and.nomsym(isym).eq.'DEPL') lpjxfem=.true._1
            endif
            if (method(1:10) .eq. 'SOUS_POINT') then
                call pjspma(corres, ch1, ch2, prol0, ligrel,&
                            noca, base, iret)
            else
                if (.not.lpjxfem) then
                  call pjxxch(corres, ch1, ch2, tychv, prfchn,&
                              prol0, ligrel, base, iret)
                else
                  if ((typres(1:4).eq.'EVOL') .or. (typres(1:4).eq.'DYNA')) then
                    call rsadpa(resu1, 'L', 1, 'INST', iordr,&
                                0, sjv=iains1, styp=kb, istop=0)
                    inst=zr(iains1)
                  else
                    inst=0.
                  endif
                  call pjxfem(corres, ch1, ch2, tychv, prfchn,&
                               prol0, ligrel, base, moa1, inst,&
                               iret)
                endif
            endif
            ASSERT(iret.eq.0.or.iret.eq.1.or.iret.eq.10)
!           ELGA ET CART : ON NE FAIT RIEN :
            if (iret .eq. 10) goto 20
!
            if (iret .gt. 0) then
                if (acceno) then
!                   L'UTILISATEUR A DEMANDE EXPLICITEMENT LA PROJECTION :
                    typerr = 'F'
                else
!                   L'UTILISATEUR N'A PAS DEMANDE EXPLICITEMENT
!                   LA PROJECTION, ON SE CONTENTE D'UNE ALARME  :
                    typerr = 'A'
                endif
                valk(1) = nomsym(isym)
                valk(2) = resu1
                valk(3) = resu2
                call utmess(typerr, 'CALCULEL4_63', nk=3, valk=valk, si=iordr)
                goto 20
            endif
            call rsnoch(resu2, nomsym(isym), iordr)
!
!           ATTRIBUTION DES ATTRIBUTS DU CONCEPT RESULTAT
!           EXTRACTION DES PARAMETRES MODAUX
            if ((typres(1:9).eq.'MODE_MECA') .or. (typres(1:4) .eq.'BASE')) then
                call vpcrea(0, resu2, ' ', ' ', ' ',&
                            prfch2(1:8), ier)
                call rsadpa(resu1, 'L', 1, 'FREQ', iordr,&
                            0, sjv=iains1, styp=kb, istop=0)
                call rsadpa(resu2, 'E', 1, 'FREQ', iordr,&
                            0, sjv=iains2, styp=kb)
                zr(iains2) = zr(iains1)
!
!               RECOPIE DE NUME_MODE S'IL EXISTE:
                call jenonu(jexnom(resu1//'           .NOVA', 'NUME_MODE'), inume)
                if (inume .ne. 0) then
                    call rsadpa(resu1, 'L', 1, 'NUME_MODE', iordr,&
                                0, sjv=iains1, styp=kb, istop=0)
                    call rsadpa(resu2, 'E', 1, 'NUME_MODE', iordr,&
                                0, sjv=iains2, styp=kb)
                    zi(iains2) = zi(iains1)
                endif
!
            else if (typres(1:9).eq.'MODE_STAT') then
                call vpcrea(0, resu2, ' ', ' ', ' ',&
                            prfch2(1:8), ier)
                call rsadpa(resu1, 'L', 1, 'NOEUD_CMP', iordr,&
                            0, sjv=iains1, styp=kb, istop=0)
                call rsadpa(resu2, 'E', 1, 'NOEUD_CMP', iordr,&
                            0, sjv=iains2, styp=kb)
                zk16(iains2) = zk16(iains1)
!
            else if (typres.eq.'DYNA_HARMO') then
                call rsadpa(resu1, 'L', 1, 'FREQ', iordr,&
                            0, sjv=iains1, styp=kb, istop=0)
                call rsadpa(resu2, 'E', 1, 'FREQ', iordr,&
                            0, sjv=iains2, styp=kb)
                zr(iains2) = zr(iains1)
!
            else if ((typres(1:4).eq.'EVOL') .or. (typres(1:4).eq.'DYNA')) then
                call rsadpa(resu1, 'L', 1, 'INST', iordr,&
                            0, sjv=iains1, styp=kb, istop=0)
                call rsadpa(resu2, 'E', 1, 'INST', iordr,&
                            0, sjv=iains2, styp=kb)
                zr(iains2) = zr(iains1)
!
            endif
!
            if (nomcmd .eq. 'DEPL_INTERNE') then
                ipar = 0
            else
!               REMPLIT D AUTRES PARAMETRES SI DEMANDE PAR UTILISATEUR
                call getvtx(' ', 'NOM_PARA', nbval=nbmax, vect=kpar, nbret=ipar)
            endif
!
!
            do ind = 1, ipar
                call rsadpa(resu1, 'L', 1, kpar(ind), iordr,&
                            1, sjv=ipar1, styp=typ1, istop=0)
                call rsadpa(resu2, 'E', 1, kpar(ind), iordr,&
                            0, sjv=ipar2, styp=typ2)
                if (typ1(1:1) .eq. 'I') then
                    zi(ipar2) = zi(ipar1)
!
                else if (typ1(1:1).eq.'R') then
                    zr(ipar2) = zr(ipar1)
!
                else if (typ1(1:2).eq.'K8') then
                    zk8(ipar2) = zk8(ipar1)
!
                else if (typ1(1:3).eq.'K16') then
                    zk16(ipar2) = zk16(ipar1)
!
                else if (typ1(1:3).eq.'K32') then
                    zk32(ipar2) = zk32(ipar1)
!
                endif
            enddo
            ico = ico + 1
!
20      continue
        enddo
    enddo
!
    if (ico .eq. 0) then
        call utmess('F', 'CALCULEL4_64')
    endif
    call jedetr('&&PJXXPR.NUME_ORDRE')
!
    if (mo2 .ne. ' ') then
        call jeveuo(resu2//'           .ORDR', 'L', jordr)
        call jelira(resu2//'           .ORDR', 'LONUTI', nbordr)
        do i = 1, nbordr
            call rsadpa(resu2, 'E', 1, 'MODELE', zi(jordr-1+i),&
                        0, sjv=jpara, styp=k8b)
            zk8(jpara)=mo2
        enddo
    endif
!   CREATION DE L'OBJET .REFD SI NECESSAIRE:
    call jeexin(resu1//'           .REFD', iret)
    if (iret .gt. 0) then
        call jeexin(resu2//'           .REFD', iret)
        if (iret .eq. 0) call refdaj(' ', resu2, -1, ' ', 'INIT',&
                                     ' ', ibid)
    endif
    call jedema()
end subroutine
