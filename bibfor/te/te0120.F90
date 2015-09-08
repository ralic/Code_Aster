subroutine te0120(nomopt, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/elref1.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/iselli.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xlsjon.h"
#include "asterfort/elrefe_info.h"
!
    character(len=16) :: nomopt, nomte
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
!.......................................................................
!
!       DEFINITION D UNE TOPOLOGIE NODALE :
!           CORRESPONDANCE ENTRE LA SUPPORT DE LA FONCTION D ENRICHISSEMENT
!           HEAVISIDE ET UN UNIQUE DOMAINE DE DISCIONTINUITE
!
!  OPTION : 'TOPONO' (X-FEM TOPOLOGIE DES DDLS HEAVISIDES AUX NOEUDS)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
    integer :: nnomax, nfismax, base_codage
    parameter (nnomax=27, nfismax=4, base_codage=4)
    character(len=8) :: elrefp, enr, elrese(6), enr2
    integer :: ibid, jcnset, jheavt, jlonch, jheano, jhease, jheafa, jheavf, jlonco
    integer :: ncomph, ncompn, ifiss, nfiss, ncmpfa, he(nfismax), nbnose(6), nface, ifac, ncmpfa2
    integer ::  id_no(nnomax), id_se, isd, nbsd, list_sd(nfismax), cpt, jfiss, jlsn, nnos, jfisco
    real(kind=8) :: heav_no(nfismax)
    aster_logical :: is_counted(base_codage**nfismax), multi_contact, pre1
    integer :: ndim, irese, nno, nnop, nse, ise, ino, iret, jtab(7)
    data          elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data          nbnose /2,3,4,3,6,10/
!
!......................................................................
!
!
    nomte=nomte
    ASSERT(nomopt.eq.'TOPONO')
!
    call teattr('S', 'XFEM', enr, ibid)
    ASSERT(enr(1:2).eq.'XH'.or.enr(1:2).eq.'XT')
!
    if (enr .eq. 'XH1C' .or. enr .eq. 'XH2C' .or. enr .eq. 'XH3C' .or. enr .eq. 'XH4C') then
        multi_contact=.true.
    else
        multi_contact=.false.
    endif
!
    call teattr('C', 'MODTHM', enr2, iret)
    pre1=(iret.eq.0)

    if (pre1.and.(enr.eq.'XH1'.or.enr.eq.'XH2'.or.enr.eq.'XH3'.or.enr.eq.'XH4')) then
        multi_contact=.true.
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! LECTURE DES DONNEES TOPOLOGIQUES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=7,&
                itab=jtab)
    ncomph = jtab(2)
    nfiss = jtab(7)  
!
    call elref1(elrefp)
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(fami='RIGI', nno=nnop, nnos=nnos, ndim=ndim)
    nno=nbnose(ndim+irese)   
!
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PLEVSET', 'L', jlsn)
    call jevech('PHEA_NO', 'E', jheano)
    call jevech('PHEA_SE', 'E', jhease)
!
    nse=zi(jlonch-1+1)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALCUL D UN NUMERO DE DOMAINE PAR NOEUD 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    id_no(1:nnomax)=0
    if (nfiss.gt.1) call jevech('PFISCO', 'L', jfisco)
!
!        write(6,*)'  ' 
!        write(6,*)' *********************************' 
!        write(6,*)' KORUPTION : VOICI LE CODAGE FISCO' 
    do ino = 1, nnop
        call xlsjon(ino, jlsn, nfiss, jfisco, heav_no(1:nfiss))
        id_no(ino)=xcalc_code(nfiss, he_real=heav_no(1:nfiss))
!        write(6,*)'     ino=',ino,' | code=',id_no(ino)
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALCUL D UN NUMERO DE DOMAINE PAR ET PAR SOUS-ELEMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    is_counted(1:base_codage**nfismax)=.false.
    nbsd=0
    do ise = 1, nse
        do ifiss = 1, nfiss
            he(ifiss)= zi(jheavt-1+ncomph*(ifiss-1)+ise)
        end do
        id_se=xcalc_code(nfiss, he(1:nfiss))
        zi(jhease-1+ise)=id_se
        if ( .not. is_counted(id_se) ) then
            is_counted(id_se)=.true.
            nbsd=nbsd+1
            ASSERT(nbsd .le. nfismax)
            list_sd(nbsd)=id_se
        endif
!        do in = 1, nno
!            ino=zi(jcnset-1+nno*(ise-1)+in)
!            if (ino .le. nnop) then
!              if (id_no(ino) .eq. 0) then
!                id_no(ino) = id_se
!              endif
!            endif
!        enddo
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   ARCHIVAGE DES SOUS-DOMAINES PAR NOEUD XFEM
!   EN POSITION 5 : LE SOUS-DOMAINE AUQUEL APPARTIENT LE NOEUD XFEM
!   LES AUTRES POSITIONS : STOCKENT LES SOUS-DOMAINES COMPLEMENTAIRES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call tecach('OOO', 'PHEA_NO', 'E', iret, nval=7,&
                itab=jtab)
    ncompn = jtab(2)/jtab(3)
    ASSERT(nbsd .le. ncompn)
    do ino = 1, nnop
      ASSERT(id_no(ino).gt.0)
      zi((jheano-1+ncompn*(ino-1)+1):(jheano-1+ncompn*(ino-1)+ncompn))=-1
      cpt=0
      zi(jheano-1+ncompn*(ino-1)+ncompn)=id_no(ino)
      do isd = 1, nbsd
        if(list_sd(isd).ne.id_no(ino)) then 
          cpt=cpt+1
          zi(jheano-1+ncompn*(ino-1)+cpt)=list_sd(isd)
        endif
     end do
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   ARCHIVAGE DES SOUS-DOMAINES PAR FACETTE
!   EN POSITION 1: LE SOUS-DOMAINE AUQUEL APPARTIENT LE COTE ESCLAVE
!   EN POSITION 2: LE SOUS-DOMAINE AUQUEL APPARTIENT LE COTE MAITRE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (multi_contact) then
        call jevech('PHEAVFA', 'L', jheavf)
        call jevech('PLONGCO', 'L', jlonco)  
        call jevech('PHEA_FA', 'E', jheafa)
        call tecach('OOO', 'PHEAVFA', 'L', iret, nval=2,&
                    itab=jtab)
        ncmpfa = jtab(2)
        call tecach('OOO', 'PHEA_FA', 'E', iret, nval=2,&
                    itab=jtab)
        ncmpfa2 = jtab(2)
        do ifiss = 1, nfiss
           nface=zi(jlonco+3*(ifiss-1)-1+2)
           do ifac = 1, nface
!     IDENTIFIANT ESCLAVE
             do jfiss=1,nfiss
               he(jfiss)=zi(jheavf-1+ncmpfa*(nfiss*(ifiss-1)+jfiss-1)+2*ifac-1)
             enddo
             zi(jheafa-1+ncmpfa2*(ifiss-1)+2*ifac-1)=xcalc_code(nfiss, he)
!     IDENTIFIANT MAITRE
             do jfiss=1,nfiss
               he(jfiss)=zi(jheavf-1+ncmpfa*(nfiss*(ifiss-1)+jfiss-1)+2*ifac)
             enddo
             zi(jheafa-1+ncmpfa2*(ifiss-1)+2*ifac)=xcalc_code(nfiss, he)
           enddo
        enddo        
    endif
!
!
    end subroutine
