subroutine aceamr(infdonn, lmax, noemaf, nbocc, infcarte, ivr )
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
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET PAR
!     MASSE REPARTIE
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE MASS_AJOU
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
!
! --------------------------------------------------------------------------------------------------
!
    use cara_elem_parameter_module
    use cara_elem_info_type
    use cara_elem_carte_type
    implicit none
    type (cara_elem_info) :: infdonn
    type (cara_elem_carte) :: infcarte(*)
    integer :: lmax, noemaf, nbocc, ivr(*)
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/affdis.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/masrep.h"
#include "asterfort/nocart.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: nbcar, nbval, nrd
    parameter    ( nbcar = 100 , nbval = 6 , nrd = 2 )
    integer :: jdc(3), jdv(3), ifm
    integer :: jdcinf, jdvinf
    integer :: i, ii, in, inbn, ino, inoe, ioc, irep
    integer :: irgno, irgto, isym, itbmp, itbno, iv
    integer :: jd, jdls, jj, jn
    integer :: l, ldgm, ldnm, lokm, lorep, nbnma
    integer :: nbno, nbnoeu, nc, ncarac, ncmp
    integer :: ndim, ng, ngp, nma, dimcar
    integer :: vali(2), nbval2
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: eta, vale(nbval)
! --------------------------------------------------------------------------------------------------
    character(len=1) :: kma(3)
    character(len=8) :: nomnoe, nommai, car(nbcar), lamass, noma
    character(len=16) :: rep, repdis(nrd)
    character(len=19) :: cart(3), cartdi
    character(len=24) :: nogp, mlgnno, mlgnma
!
    aster_logical :: transl, lvale
    integer :: iarg
    data repdis  /'GLOBAL          ','LOCAL           '/
    data kma     /'K','M','A'/
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    noma   = infdonn%maillage
    ndim   = infdonn%dimmod
!   Pour les discrets c'est obligatoirement du 2D ou 3D
    ASSERT((ndim.eq.2).or.(ndim.eq.3))
!
    nbval2 = 3
!
    mlgnno = noma//'.NOMNOE'
    mlgnma = noma//'.NOMMAI'
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPTABNO', 'V V K8', lmax, itbno)
    call wkvect('&&TMPRIGNO', 'V V R', 6*lmax, irgno)
    call wkvect('&&TMPRIGTO', 'V V R', 6*noemaf, irgto)
    call wkvect('&&TMPTABMP', 'V V K8', lmax, itbmp)
!
!   Les cartes sont déjà construites : ace_crea_carte
    cartdi = infcarte(ACE_CAR_DINFO)%nom_carte
    jdcinf = infcarte(ACE_CAR_DINFO)%adr_cmp
    jdvinf = infcarte(ACE_CAR_DINFO)%adr_val
    dimcar = infcarte(ACE_CAR_DINFO)%nbr_cmp
!
    cart(1) = infcarte(ACE_CAR_DISCK)%nom_carte
    jdc(1)  = infcarte(ACE_CAR_DISCK)%adr_cmp
    jdv(1)  = infcarte(ACE_CAR_DISCK)%adr_val
!
    cart(2) = infcarte(ACE_CAR_DISCM)%nom_carte
    jdc(2)  = infcarte(ACE_CAR_DISCM)%adr_cmp
    jdv(2)  = infcarte(ACE_CAR_DISCM)%adr_val
!
    cart(3) = infcarte(ACE_CAR_DISCA)%nom_carte
    jdc(3)  = infcarte(ACE_CAR_DISCA)%adr_cmp
    jdv(3)  = infcarte(ACE_CAR_DISCA)%adr_val
!
    ifm = ivr(4)
!
!   Boucle sur les occurrences de MASS_AJOU
    do ioc = 1, nbocc
        eta = 0.0d0
!       Par défaut on est dans le repère global, matrices symétriques
        irep = 1; isym = 1
        rep = repdis(1)
        lvale=.false.
!
        call getvem(noma, 'GROUP_MA', 'MASS_AJOU', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call r8inir(nbval, 0.0d0, vale, 1)
        call getvtx('MASS_AJOU', 'GROUP_MA_POI1', iocc=ioc, scal=nogp, nbret=ngp)
!
        do i = 1, nrd
            if (rep .eq. repdis(i)) irep = i
        enddo
!
        ncarac=1
        if (ivr(3).eq.2) then
            write(ifm,100) rep,ioc
        endif
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
        if (ng .le. 0) goto 30
        car(1)='M_T_N'
!
!         II = 0
        do nc = 1, ncarac
            transl=.true.
!
            if (transl) then
                lamass = 'K_T_D_N'
                call masrep(noma, ioc, vale, lvale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto), ndim)
            else
                ASSERT(.false.)
            endif
!
            do ino = 1, nbno
                zk8(itbmp + ino - 1) = ' '
            enddo
!
!
!
            if (ngp .ne. 0) then
                nbnoeu = 1
                lokm = 5
!
                call jelira(jexnom(noma//'.GROUPEMA', nogp), 'LONMAX', nma)
                call jeveuo(jexnom(noma//'.GROUPEMA', nogp), 'L', ldgm)
!
                if (nma .ne. nbno) then
                    vali(1) = nbno
                    vali(2) = nma
                    call utmess('F', 'MODELISA2_10', sk=nogp, ni=2, vali=vali)
                endif
!
!
                do in = 0, nma-1
!                 RECUPERE LE NOMBRE DE NOEUD DE LA MAILLE
                    call jelira(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'LONMAX', nbnma)
                    call jeveuo(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'L', ldnm)
                    call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
!                 BOUCLE SUR LE NB DE NOEUD DE LA MAILLE
                    if (nbnma .ne. nbnoeu) then
                        call utmess('F', 'MODELISA_20', sk=nommai)
                    endif
                    do inbn = 1, nbnma
                        inoe = zi(ldnm+inbn-1)
                        call jenuno(jexnum(mlgnno, inoe), nomnoe)
                        do ino = 1, nbno
                            if (zk8(itbno+ino-1) .eq. nomnoe) then
                                zk8(itbmp+ino-1) = nommai
!
                                goto 22
                            endif
                        enddo
                    enddo
!                 SI ON PASSE ICI AUCUN DES NOEUDS DU DISCRET APPARTIENT
!                 A LA SURFACE, ET CE N'EST PAS NORMAL
                    write(ifm,*)'GROUP_MA :', (' '//zk24(jdls+ii-1), ii=1,ng)
                    call utmess('F', 'MODELISA_21', sk=nomnoe)
 22                 continue
                enddo
!              PREPARATION DES IMPRESSIONS DANS LE FICHIER MESSAGE
                lorep = 5
                if (irep .eq. 1) lorep = 6
!
!
!            VERIF QU'UN DISCRET EST FIXE A CHACUN DES NOEUDS DU RADIER
!            (UNE SEULE FOIS PAR OCCURRENCE DE MASS_AJOU)
                if (nc .eq. 1) then
                    do ino = 1, nbno
                        if (zk8(itbmp + ino - 1) .eq. ' ') then
                            call jenuno(jexnum(mlgnno, ino), nomnoe)
                            call utmess('F', 'MODELISA2_8', sk=nomnoe)
                        endif
                    enddo
                endif
!
                if (ivr(3).eq.2) then
                    do i = 1, nbno
                        iv = 1
                        jd = itbmp + i - 1
                        jn = itbno + i - 1
                        if (nbnoeu .eq. 1) then
                            if (transl) then
                                write(ifm,111) 'MAILLE',zk8(jn),&
                                    car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                    jj=0,5), repdis(irep)(1:lorep)
                            endif
                        endif
                    enddo
                endif
!
                do i = 1, nbno
                    iv = 1
                    jd = itbmp + i - 1
                    jn = itbno + i - 1
!
                    call affdis(ndim, irep, eta, car(nc), zr(irgno+6*i-6),&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym )
                    call nocart(cartdi, 3, dimcar, mode='NOM', nma=1, limano=[zk8(jd)])
                    call nocart(cart(l), 3, ncmp, mode='NOM', nma=1, limano=[zk8(jd)])
                enddo
            endif
        enddo
!
 30     continue
    enddo
!
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPTABNO')
    call jedetr('&&TMPRIGNO')
    call jedetr('&&TMPRIGTO')
    call jedetr('&&TMPTABMP')
!
    call jedema()
!
100 format(/,&
     &    ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',&
     &    '(REPERE ',a6,'), OCCURRENCE ',i4)
!
111 format(' _F(',a,'=''',a8,''', CARA=''',a,''',',/,&
     &       '   VALE=(',3(1x,1pe12.5,','),/,&
     &       '         ',3(1x,1pe12.5,','),'),',/,&
     &       '   REPERE=''',a,'''),')
end subroutine
