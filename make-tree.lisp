;;; make-tree.lisp (pseudocode)
;;; 02-Feb-2018 SVS

(defparameter *nodelist* '("128.20.100.221."
                           "38.241.145.66"
                           "205.56.236.194"
                           "52.5.106.49"
                           "6.158.192.181"
                           "36.233.64.61"
                           "149.67.194.204"
                           "164.91.61.109"
                           "180.171.72.115"
                           "30.73.245.135"
                           )
  "List of nodes given from <deity>. Real list will probably have public keys too. Omit for now.")

(defparameter *standard-port* 5555 "Standard listener port for admin messages.")
(defparameter *deminimus* 3 "If there are fewer nodes than this, do not create a new level in the tree")
(defparameter *max-fanout* 5 "Max fanout from each node")

(defun make-child-node (parent childIP nodelist)
  (send-admin-message childIP *standard-port* 'make-tree parent nodelist))

(defun divide-into-subsets (n nodelist)
  "aye, here be magic.
   Returns a list of n sublists of nodelist by some algorithm."
  )

(defun make-tree (parent nodelist)
  (unless (member (localhost-ip-address) nodelist :key 'ipaddr)
    (error "~S received make-tree message but it is not in the nodelist" (localhost-ip-address)))
  (when (find-local-tree-node) ; should this be an error?
    (error "~S received make-tree message but it already has a local tree node" (localhost-ip-address)))
  (let ((local-node (make-local-tree-node (localhost-ip-address))))
    ; after the above, find-local-tree-node will return local-node
    (setf nodelist (remove (localhost-ip-address) nodelist :key 'ipaddr)) ; because I just made this node!
    (let ((subsets
           (if (< (length nodelist) *deminimus*)
               (list nodelist) ; just make one subset, which is the whole nodelist
               (divide-into-subsets *max-fanout* nodelist))))
      (dolist (subset subsets)
        (make-child-node (localhost-ip-address) (ipaddr (first subset)) subset))
      (reply-upward local-node
                    (format t "Success creating subtree at ~S!" (localhost-ip-address))))))
      
      
