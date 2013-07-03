subroutine rvouex(mcf, iocc, nchpt, lstcmp, lstmac,&
                  lstnac, iret)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/celcel.h"
#include "asterfort/celver.h"
#include "asterfort/cncinv.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/i2fnoe.h"
#include "asterfort/i2trgi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/rvfmai.h"
#include "asterfort/rvgnoe.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utmach.h"
#include "asterfort/utncmp.h"
#include "asterfort/wkvect.h"
    character(len=24) :: lstcmp, lstmac, lstnac
    character(len=*) :: mcf, nchpt
    integer :: iocc, iret
!**********************************************************************
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
!  OPERATION REALISEE
!  ------------------
!     CONSTRUCTION DES VECTEURS DES MAILLES ET/OU NOEUDS ACTIFS
!
!  ARGUMENTS EN ENTREE
!  -------------------
!     IOCC   : NUMERO DE L' OCCURENCE TRAITEE
!     NCHPT  : NOM DU CHAM_GD A TRAITER
!     LSTCMP : NOM DU VECTEUR DES NUMEROS DE CMP MISES EN JEU
!
!  ARGUMENTS EN SORTIE
!  -------------------
!     LSTMAC : NOM DE L' OJB 'V V I' DES NUMEROS DE MAILLES ACTIVES
!     LSTNAC : NOM DE L' OJB 'V V I' DES NUMEROS DE NOEUDS ACTIFS
!     IRET   : CODE RETOUR 1 = OK, 0 = KO
!
!  REMARQUE
!  --------
!     SUIVANT LA NATURE DU CHAMP TRAITE UN SEUL DES OJB LSTMAC ET
!     LSTNAC EST CONSTRUIT
!
!**********************************************************************
!
!
!
!  VARIABLES LOCALES
!  -----------------
    integer :: adr, aliste, acncin, alsmac, alsnac, acmp, adrvlc, arepe
    integer :: nbtma, nbm, nbmac, nbnac, nbcrb, nbmalu
    integer :: i, in, n, m, libre, n1, ibid, igrel, jnuma, j
    integer :: ibib, ie, imolo, jceld, n2, kk, ier, nbvari, nbr
    integer :: ii, jmmail, nbtrou, nbcmp, nbcmp1, nc, jcmp, jcmp1, ntc
    character(len=4) :: docu
    character(len=8) :: nmaila, courbe, k8b, nomgd, resuco, nomvar, num
    character(len=15) :: nconec
    character(len=16) :: motcle(2), typmcl(2), nchsym
    character(len=19) :: nchp19
    character(len=24) :: ncncin, nrepe, lismai, malist, nomobj, valk(3)
    integer :: iarg
    data nbvari /100/
!**********************************************************************
!
    call jemarq()
!
    call jeveuo(jexnum(lstcmp, iocc), 'L', acmp)
    malist = '&&RVOUEX_MALIST'
!
    call getvid(mcf, 'CHEMIN', iocc, iarg, 0,&
                zk8, nbcrb)
    nbcrb = -nbcrb
    if (nbcrb .ne. 0) then
        call getvid(mcf, 'CHEMIN', iocc, iarg, nbcrb,&
                    courbe, ibib)
    endif
!
    nchp19 = nchpt
    iret = 1
!
    if (nchp19(1:1) .ne. '&') then
!
        call jeexin(nchp19//'.DESC', ibid)
        if (ibid .gt. 0) then
            call jelira(nchp19//'.DESC', 'DOCU', ibid, docu)
        else
!
!          -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
!
            call celcel('NBVARI_CST', nchp19, 'V', '&&RVOUEX.CHAMEL1')
            nchp19= '&&RVOUEX.CHAMEL1'
            call celver(nchp19, 'NBSPT_1', 'COOL', kk)
            if (kk .eq. 1) then
                call dismoi('F', 'NOM_GD', nchp19, 'CHAMP', ibid,&
                            nomgd, ie)
                call u2mesk('I', 'PREPOST_36', 1, nomgd)
                call celcel('PAS_DE_SP', nchp19, 'V', '&&RVOUEX.CHAMEL2')
                nchp19= '&&RVOUEX.CHAMEL2'
            endif
            call jelira(nchp19//'.CELD', 'DOCU', ibid, docu)
        endif
!
        call dismoi('F', 'NOM_MAILLA', nchp19, 'CHAMP', ibid,&
                    nmaila, ie)
        nconec = nmaila//'.CONNEX'
        ncncin = '&&OP0051.CONNECINVERSE  '
!
        call jelira(nconec, 'NMAXOC', nbtma, k8b)
!
        nbtrou = 0
        jmmail = 1
!
        call getvtx(mcf, 'NOM_CMP', iocc, iarg, 0,&
                    k8b, nc)
        if (nc .lt. 0 .and. nbcrb .eq. 0) then
            nbcmp = -nc
            call wkvect('&&RVOUEX.NOM_CMP', 'V V K8', nbcmp, jcmp)
            call getvtx(mcf, 'NOM_CMP', iocc, iarg, nbcmp,&
                        zk8(jcmp), nc)
!
! VERIFICATION QUE LES COMPOSANTES DEMANDEES
! APPARTIENNENT BIEN AU CHAMP
!
            call getvid(mcf, 'RESULTAT', iocc, iarg, 1,&
                        resuco, ibib)
            if (ibib .ne. 0) then
                nomobj = '&&RVOUEX.NOM_CMP1'
                call jeexin(nomobj, ier)
                if (ier .ne. 0) call jedetr(nomobj)
                call utncmp(nchp19, nbcmp1, nomobj)
                call jeveuo(nomobj, 'L', jcmp1)
                call getvtx(mcf, 'NOM_CHAM', iocc, iarg, 1,&
                            nchsym, n1)
                nbr=nbcmp1
                if (zk8(jcmp1) .eq. 'VARI') nbr=nbvari
                do 102 i = 1, nbcmp
                    do 103 j = 1, nbr
                        if (zk8(jcmp1) .eq. 'VARI') then
                            call codent(j, 'G', num)
                            nomvar = 'V'//num
                            if (zk8(jcmp-1+i) .eq. nomvar) goto 102
                        else
                            if (zk8(jcmp-1+i) .eq. zk8(jcmp1-1+j)) goto 102
                        endif
103                  continue
                    valk(1) = zk8(jcmp-1+i)
                    valk(2) = nchsym
                    valk(3) = resuco
                    call u2mesk('F', 'POSTRELE_65', 3, valk)
102              continue
            endif
!
            call utmach(nchp19, nbcmp, zk8(jcmp), 'NU', malist,&
                        nbtrou)
            if (nbtrou .ne. 0) call jeveuo(malist, 'L', jmmail)
            call jedetr('&&RVOUEX.NOM_CMP')
        endif
!
        call getvtx(mcf, 'TOUT_CMP', iocc, iarg, 0,&
                    k8b, ntc)
        if (ntc .lt. 0 .and. nbcrb .eq. 0) then
            nomobj = '&&RVOUEX.NOMCMP.USER'
            call utncmp(nchp19, nbcmp, nomobj)
            call jeveuo(nomobj, 'L', jcmp)
            call utmach(nchp19, nbcmp, zk8(jcmp), 'NU', malist,&
                        nbtrou)
            if (nbtrou .ne. 0) call jeveuo(malist, 'L', jmmail)
            call jedetr(nomobj)
        endif
!
        if (docu .eq. 'CHML') then
!             ----------------
            call jeveuo(nchp19//'.CELK', 'L', adr)
            nrepe = zk24(adr)(1:19)//'.REPE'
            call jeveuo(nrepe, 'L', arepe)
!
            if (nbcrb .ne. 0) then
!
                call rvfmai(courbe, lstmac)
!
            else
!
                call rvgnoe(mcf, iocc, nmaila, lstnac, 0,&
                            ibid)
!
                call getvtx(mcf, 'GROUP_MA', iocc, iarg, 0,&
                            k8b, n1)
                call getvtx(mcf, 'MAILLE', iocc, iarg, 0,&
                            k8b, n2)
                if ((n1+n2) .eq. 0) then
                    nbmalu = 0
                else
                    lismai = '&&RVOUEX.NUME_MAIL'
                    motcle(1) = 'GROUP_MA'
                    motcle(2) = 'MAILLE'
                    typmcl(1) = 'GROUP_MA'
                    typmcl(2) = 'MAILLE'
                    call reliem(' ', nmaila, 'NU_MAILLE', mcf, iocc,&
                                2, motcle, typmcl, lismai, nbmalu)
                    call jeveuo(lismai, 'L', jnuma)
                endif
!
                call jeexin(ncncin, n2)
                if (n2 .eq. 0) call cncinv(nmaila, ibid, 0, 'V', ncncin)
!
                call jelira(lstnac, 'LONMAX', nbnac, k8b)
                call jeveuo(lstnac, 'L', alsnac)
!
                call jecreo('&&RVOUEX.LISTE.ENTIER', 'V V I')
                call jeecra('&&RVOUEX.LISTE.ENTIER', 'LONMAX', nbtma, ' ')
                call jeveuo('&&RVOUEX.LISTE.ENTIER', 'E', aliste)
!
                libre = 1
                call jeveuo(jexatr(ncncin, 'LONCUM'), 'L', adrvlc)
                call jeveuo(jexnum(ncncin, 1), 'L', acncin)
!
                do 100, in = 1, nbnac, 1
                n = zi(alsnac + in-1)
                nbm = zi(adrvlc + n+1-1) - zi(adrvlc + n-1)
                adr = zi(adrvlc + n-1)
!
                call i2trgi(zi(aliste), zi(acncin + adr-1), nbm, libre)
!
100              continue
!
                nbmac = libre - 1
                libre = 1
!
                call jeveuo(nchp19//'.CELD', 'L', jceld)
!
                do 110, i = 1, nbmac, 1
                m = zi(aliste + i-1)
                if (nbtrou .ne. 0) then
                    do 112 ii = 1, nbtrou
                        if (m .eq. zi(jmmail+ii-1)) goto 114
112                  continue
                    goto 110
114                  continue
                endif
                if (m .ne. 0) then
                    if (nbmalu .ne. 0) then
                        do 402, j = 1, nbmalu, 1
                        if (m .eq. zi(jnuma+j-1)) goto 404
402                      continue
                        goto 110
404                      continue
                    endif
                    igrel = zi(arepe + 2*(m-1))
                    imolo=zi(jceld-1+zi(jceld-1+4+igrel) +2)
                    if (igrel .ne. 0 .and. imolo .gt. 0) then
                        zi(aliste + libre-1) = zi(aliste + i-1)
                        libre = libre + 1
                    endif
                endif
110              continue
!
                nbmac = libre - 1
!
                if (nbmac .gt. 0) then
!
                    call wkvect(lstmac, 'V V I', nbmac, alsmac)
!
                    do 120, i = 1, nbmac, 1
                    zi(alsmac + i-1) = zi(aliste + i-1)
120                  continue
!
                else
!
                    iret = 0
!
                endif
            endif
!
            call jedetr('&&RVOUEX.LISTE.ENTIER')
            call jedetr('&&RVOUEX.NUME_MAIL')
!
        else
!             ----------------
!
            if (nbcrb .ne. 0) then
!
                call i2fnoe(courbe, lstnac)
!
            else
!
                call rvgnoe(mcf, iocc, nmaila, lstnac, nbtrou,&
                            zi(jmmail))
!
            endif
!
        endif
!
    endif
!
    call jedetr(malist)
    call detrsd('CHAM_ELEM', '&&RVOUEX.CHAMEL1')
    call detrsd('CHAM_ELEM', '&&RVOUEX.CHAMEL2')
    call jedema()
end subroutine
