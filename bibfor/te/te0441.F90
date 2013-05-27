subroutine te0441(option, nomte)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/teattr.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xpesro.h'
    include 'asterfort/xteddl.h'
    include 'asterfort/xteini.h'
    character(len=16) :: option, nomte
!......................................................................
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTIONS  CHAR_MECA_PESA_R ET CHAR_MECA_ROTA_R
!                          POUR LES ÉLÉMENTS X-FEM
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
    integer :: j, kk, ndim, nno, nnop, nnops, nnos, nnom, nddl, npg, singu
    integer :: nfh, ddls, nfe, ddlc, nse, ise, in, ino, ibid, ddlm
    integer :: jpintt, jcnset, jheavt, jlonch, jlsn, jlst, jstno, jpmilt
    integer :: ivectu, igeom, irota, ipesa, imate
    integer :: irese, nfiss, jfisno, kpg, spt
    real(kind=8) :: rbid, fno(81), rho, om, omo, coorse(81)
    integer :: icodre(3)
    character(len=8) :: elrefp, elrese(6), fami(6), enr, lag, famil, poum
    character(len=16) :: phenom
    logical :: lbid
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','RIGI','XINT','BID','RIGI','XINT'/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ndim, nnop, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
    call elref4(elrese(ndim+irese), fami(ndim+irese), ibid, nno, nnos,&
                npg, ibid, ibid, ibid, ibid)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                ibid)
!
!     PARAMETRE DU VECTEUR ELEMENTAIRE
    call jevech('PVECTUR', 'E', ivectu)
!
!     PARAMÈTRES PROPRES À X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PSTANO', 'L', jstno)
    call jevech('PMATERC', 'L', imate)
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (nomte(3:4).ne.'AX') .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
!     PARAMETRE MATERIAU : RHO MASSE VOLUMIQUE
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(famil, kpg, spt, poum, zi(imate),&
                ' ', phenom, 1, ' ', rbid,&
                1, 'RHO', rho, icodre, 1)
!     CALCUL DE L'EFFORT VOLUMIQUE AUX NOEUDS DE L'ELEMENT PARENT : FNO
    call vecini(ndim*nnop, 0.d0, fno)
!
    if (option .eq. 'CHAR_MECA_PESA_R') then
!
        call jevech('PPESANR', 'L', ipesa)
!
        do 10 ino = 1, nnop
            do 11 j = 1, ndim
                kk = ndim*(ino-1)+j
                fno(kk) = fno(kk) + rho*zr(ipesa)*zr(ipesa+j)
11          continue
10      continue
!
    else if (option.eq.'CHAR_MECA_ROTA_R') then
!
        call jevech('PROTATR', 'L', irota)
!
        om = zr(irota)
        do 20 ino = 1, nnop
            omo = 0.d0
            do 21 j = 1, ndim
                omo = omo + zr(irota+j)* zr(igeom+ndim*(ino-1)+j-1)
21          continue
            do 22 j = 1, ndim
                kk = ndim*(ino-1)+j
                fno(kk)=fno(kk)+rho*om*om*(zr(igeom+kk-1)-omo*zr(&
                irota+j))
22          continue
20      continue
!
    endif
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        do 111 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
112          continue
111      continue
!
        call xpesro(elrefp, ndim, coorse, igeom, jheavt,&
                    jfisno, nfh, ddlc, nfe, nfiss,&
                    ise, nnop, jlsn, jlst, ivectu,&
                    fno)
!
!
110  end do
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call teattr(nomte, 'C', 'XLAG', lag, ibid)
    if (ibid .eq. 0 .and. lag .eq. 'ARETE') then
        nnop = nnos
    endif
    call xteddl(ndim, nfh, nfe, ddls, nddl,&
                nnop, nnops, zi(jstno), .false., lbid,&
                option, nomte, rbid, zr(ivectu), ddlm,&
                nfiss, jfisno)
!
    call jedema()
end subroutine
