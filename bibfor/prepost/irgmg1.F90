subroutine irgmg1(numold, ima, nbord2, tabd, tabl,&
                  tabv, partie, jtype, nbno, icmp,&
                  ifi, iwri, iadmax)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: numold(*), tabd(*), tabl(*), tabv(*), jtype
    integer :: icmp, ifi, ima, nbord2, iadmax, nbno
    aster_logical :: iwri
    character(len=*) :: partie
!     ------------------------------------------------------------------
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
!     BUT: ECRITURE D'UNE CMP D'UN CHAMP "ELGA" OU "ELEM"
!     POUR UN TYPE D'ELEMENT AU FORMAT GMSH
!
!     ENTREE:
!     NUMOLD : I   : TABLEAU DE CORRESPONDANCE NOUV MAILLE ANC. MAILLE
!     IMA    : I   : NUMERO NOUVELLE MAILLE
!     NBORD2 : I   : NOMBRE DE NUM D'ORDRE
!     TABD   : I   : DECRIPTEURS DU CHMAP SIMPLE A IMMRIMER
!     TABL   : I   : DECRIPTEURS DU CHMAP SIMPLE A IMMRIMER
!     TABV   : I   : DECRIPTEURS DU CHMAP SIMPLE A IMMRIMER
!     PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!     JTYPE  : I   : ADRESSE DU TYPE DU CHAMP ( REEL OU COMPLEXE )
!     ICMP   : I   : NUMERO COMPOSANTE CHAMP
!     IFI    : I   : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
!     IWRI    : L   : INDIQUE SI ON DOIT ECRIRE
!     SORTIE
!     IADMAX  : I   : MAX DES IAD SI >0 LE CHAMP EXISTE POUR LA MAILLE
!
!     ------------------------------------------------------------------
    integer :: imaold, ior, jcesd, jcesl, jcesv, nbpt, nbsp, ipt, isp, iad, ino
    real(kind=8) :: vale
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    imaold = numold(ima)
!
!     ON NE TRAITE QUE LES CHAMPS A 1 SOUS-POINT,
!     ET UNE SEULE VALEUR SCALAIRE
!
    isp = 1
    iadmax = 0
!
    do 11 ior = 1, nbord2
        jcesd = tabd(ior)
        jcesl = tabl(ior)
        jcesv = tabv(ior)
        nbpt = zi(jcesd-1+5+4*(imaold-1)+1)
        nbsp = zi(jcesd-1+5+4*(imaold-1)+2)
        if (nbsp .ne. 1) then
            call utmess('F', 'PREPOST2_57')
        endif
        vale = 0.d0
        if (zk8(jtype-1+ior) .eq. 'R') then
            do 13 ipt = 1, nbpt
                call cesexi('C', jcesd, jcesl, imaold, ipt,&
                            isp, icmp, iad)
                if (iad .gt. 0) then
                    iadmax = iad
                    vale = vale + zr(jcesv-1+iad)
                endif
 13         continue
        else if (zk8(jtype-1+ior).eq.'C') then
            if (partie .eq. 'REEL') then
                do 15 ipt = 1, nbpt
                    call cesexi('C', jcesd, jcesl, imaold, ipt,&
                                isp, icmp, iad)
                    if (iad .gt. 0) then
                        iadmax = iad
                        vale = vale + dble(zc(jcesv-1+iad))
                    endif
 15             continue
            else if (partie.eq.'IMAG') then
                do 17 ipt = 1, nbpt
                    call cesexi('C', jcesd, jcesl, imaold, ipt,&
                                isp, icmp, iad)
                    if (iad .gt. 0) then
                        iadmax = iad
                        vale = vale + dimag(zc(jcesv-1+iad))
                    endif
 17             continue
            endif
        endif
        if (abs(vale) .le. 1.d-99) vale = 0.d0
        if (nbpt .ne. 0) vale = vale / nbpt
        if (iwri) then
            do 19 ino = 1, nbno
                write(ifi,1000) vale
 19         continue
        endif
 11 end do
!
    call jedema()
!
    1000 format(1p,e15.7e3)
!
end subroutine
